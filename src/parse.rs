use std::collections::HashMap;
use regex::Regex;

pub struct Docopt {
    program: String,
    usages: Alternates,
    descs: HashMap<String, AtomDesc>,
    short_to: HashMap<String, String>,
    last_desc_added: Option<String>,
}

type Alternates = Vec<Pattern>;
struct Pattern { pats: Vec<Repeatable> }
struct Repeatable { repeats: bool, expr: Expr }

enum Expr {
    Grouped(Alternates),
    Optional(Alternates),
    Options,
    Atom(String), // key in Docopt.descs
}

struct AtomDesc {
    name: String,
    
    /// Set to true if this atom is ever repeated in any context.
    /// For positional arguments, non-argument flags and commands, repetition 
    /// means that they become countable.
    /// For flags with arguments, repetition means multiple distinct values
    /// can be specified (and are represented as a Vec).
    repeats: bool,
    desc: AtomType,
}

enum AtomType {
    Arg, // positional
    Command,
    Flag(ArgDesc),
}

#[deriving(Show, PartialEq)]
enum ArgDesc {
    Zero,
    One,
    // Default implies One
    Default(String),
}

macro_rules! err(
    ($($arg:tt)*) => (return Err(format!($($arg)*)))
)

static USAGE: Regex = regex!(r"(?is)usage:\s+(?P<prog>\S+)(?P<pats>.*?)\n\s*\n");

impl Docopt {
    pub fn new(doc: &str) -> Result<Docopt, String> {
        let mut d = Docopt {
            program: "".to_string(),
            usages: vec!(),
            descs: HashMap::new(),
            short_to: HashMap::new(),
            last_desc_added: None,
        };
        try!(d.parse(doc));
        Ok(d)
    }

    fn parse(&mut self, doc: &str) -> Result<(), String> {
        let caps = match USAGE.captures(doc) {
            None => err!("Could not find usage patterns in doc string."),
            Some(caps) => caps,
        };
        let prog = ::regex::quote(caps.name("prog"));
        if prog.len() == 0 {
            err!("Could not find program name in doc string.")
        }

        // Before we parse the usage patterns, we look for option descriptions.
        // We do this because the information in option descriptions can be
        // used to resolve ambiguities in usage patterns (i.e., whether
        // `--flag ARG` is a flag with an argument or not).
        //
        // From the docopt page, "every" line starting with a `-` or a `--`
        // is considered an option description. Instead, we restrict the lines
        // to any line *not* in the usage pattern section.
        let (pstart, pend) = caps.pos(0).unwrap();
        let (before, after) = (doc.slice_to(pstart), doc.slice_from(pend));
        // We process every line here (instead of restricting to lines starting
        // with "-") because we need to check every line for a default value.
        // The default value always belongs to the most recently defined desc.
        for line in before.lines().chain(after.lines()) {
            try!(self.parse_desc(line));
        }

        let pats = Regex::new(format!("(?s)(.*?)({}|$)", prog).as_slice()).unwrap();
        for _ in pats.captures_iter(caps.name("pats")) {
            // println!("${}$", pat.at(1).trim()); 
        }
        Ok(())
    }

    fn parse_desc(&mut self, desc: &str) -> Result<(), String> {
        let desc = desc.trim();
        if !regex!(r"^(-\S|--\S)").is_match(desc) {
            // TODO: Check for default value.
            return Ok(())
        }
        // Get rid of the description, which must be at least two spaces
        // after the flag or argument.
        let desc = regex!("  .*$").replace(desc, "");
        // Normalize `-x, --xyz` to `-x --xyz`.
        let desc = regex!(r"([^-\s]), -").replace(desc.as_slice(), "$1 -");

        let rflags = regex!(r"(?:(?P<long>--[^ \t=]+)|(?P<short>-[^ \t=]+))(?:(?: |=)(?P<arg>[^-]\S+))?");
        let (mut short, mut long) = ("".to_string(), "".to_string());
        let mut arg = Zero;
        for flags in rflags.captures_iter(desc.as_slice()) {
            let (s, l) = (flags.name("short"), flags.name("long"));
            if s.len() > 0 { short = s.to_string() }
            if l.len() > 0 { long = l.to_string() }
            if arg == Zero && flags.name("arg").len() > 0 {
                arg = One;
            }
        }
        self.add_desc(short, long, arg)
    }

    fn add_desc(&mut self, short: String, long: String, arg: ArgDesc) -> Result<(), String> {
        assert!(!short.is_empty() || !long.is_empty());
        if !short.is_empty() && short.len() != 2 {
            err!("Short flag '{}' is not of the form '-x'.", short);
        }
        if self.lookup(short.as_slice()).is_some() {
            err!("Short flag '{}' is defined more than once.", short);
        }
        if self.lookup(long.as_slice()).is_some() {
            err!("Long flag '{}' is defined more than once.", long);
        }
        let mut canonical = long.clone();
        if long.is_empty() {
            canonical = short.clone();
        }
        if !short.is_empty() {
            self.short_to.insert(short, canonical.clone());
        }
        self.descs.insert(canonical.clone(), AtomDesc {
            name: canonical,
            repeats: false, // not known yet; discovered in usage patterns
            desc: Flag(arg), // arg desc may change if default value is found
        });
        Ok(())
    }

    fn lookup<'a>(&'a mut self, flag: &str) -> Option<&'a AtomDesc> {
        let key =
            if is_short(flag) {
                match self.short_to.find_equiv(&flag) {
                    Some(long) => long.as_slice(),
                    None => return None,
                }
            } else {
                flag
            };
        self.descs.find_equiv(&key)
    }
}

fn is_short(s: &str) -> bool { return regex!(r"^-[^-]+$").is_match(s) }
fn is_long(s: &str) -> bool { return regex!(r"^--\S+$").is_match(s) }
fn is_arg(s: &str) -> bool { return regex!(r"^\p{Lu}+|<[^>]+>$").is_match(s) }
fn is_cmd(s: &str) -> bool { return regex!(r"^(-|--|[^-]\S+)$").is_match(s) }
