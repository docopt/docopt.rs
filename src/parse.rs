use std::collections::HashMap;
use std::fmt;
use regex;
use regex::Regex;

pub struct Docopt {
    program: String,
    usages: Alternates,
    descs: HashMap<String, AtomDesc>,
    short_to: HashMap<String, String>,
    last_desc_added: Option<String>,
}

impl fmt::Show for Docopt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        try!(writeln!(f, "====="))
        try!(writeln!(f, "Program: {}", self.program))
        try!(writeln!(f, "Option descriptions:"))
        for (k, v) in self.descs.iter() {
            try!(writeln!(f, "  '{}' => {}", k, v))
        }
        try!(writeln!(f, "Short abbreviations:"))
        for (k, v) in self.short_to.iter() {
            try!(writeln!(f, "  '{}' => '{}'", k, v))
        }
        writeln!(f, "=====")
    }
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

#[deriving(Show)]
struct AtomDesc {
    name: String,
    
    /// Set to true if this atom is ever repeated in any context.
    /// For positional arguments, non-argument flags and commands, repetition 
    /// means that they become countable.
    /// For flags with arguments, repetition means multiple distinct values
    /// can be specified (and are represented as a Vec).
    repeats: bool,
    atype: AtomType,
}

#[deriving(Show)]
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
        let musage = regex!(r"(?is)usage:\s+(?P<prog>\S+)(?P<pats>.*?)\n\s*\n");
        let caps = match musage.captures(doc) {
            None => err!("Could not find usage patterns in doc string."),
            Some(caps) => caps,
        };
        if caps.name("prog").is_empty() {
            err!("Could not find program name in doc string.")
        }
        self.program = caps.name("prog").to_string();

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

        let mprog = format!("(?s)(.*?)({}|$)", regex::quote(caps.name("prog")));
        let pats = Regex::new(mprog.as_slice()).unwrap();
        for _ in pats.captures_iter(caps.name("pats")) {
            // println!("${}$", pat.at(1).trim()); 
        }
        Ok(())
    }

    fn parse_desc(&mut self, full_desc: &str) -> Result<(), String> {
        let desc = full_desc.trim();
        if !regex!(r"^(-\S|--\S)").is_match(desc) {
            try!(self.parse_default(full_desc));
            return Ok(())
        }
        // Get rid of the description, which must be at least two spaces
        // after the flag or argument.
        let desc = regex!("  .*$").replace(desc, "");
        // Normalize `-x, --xyz` to `-x --xyz`.
        let desc = regex!(r"([^-\s]), -").replace(desc.as_slice(), "$1 -");
        let desc = desc.as_slice().trim();

        let rflags = regex!("(?:(?P<long>--[^ \t=]+)|(?P<short>-[^ \t=]+))\
                             (?:(?: |=)(?P<arg>[^-]\\S*))?");
        let (mut short, mut long) = ("".to_string(), "".to_string());
        let mut arg = Zero;
        let mut last_end = 0;
        for flags in rflags.captures_iter(desc) {
            last_end = flags.pos(0).unwrap().val1();
            let (s, l) = (flags.name("short"), flags.name("long"));
            if !s.is_empty() {
                if !short.is_empty() {
                    err!("Only one short flag is allowed in an option \
                          description, but found '{}' and '{}'.", short, s)
                }
                short = s.to_string()
            }
            if !l.is_empty() {
                if !long.is_empty() {
                    err!("Only one long flag is allowed in an option \
                          description, but found '{}' and '{}'.", long, l)
                }
                long = l.to_string()
            }
            if flags.name("arg").len() > 0 {
                try!(err_if_invalid_arg(flags.name("arg")));
                arg = One; // may be changed to default later
            }
        }
        // Make sure that we consumed everything. If there are leftovers,
        // then there is some malformed description. Alert the user.
        assert!(last_end <= desc.len());
        if last_end < desc.len() {
            err!("Extraneous text '{}' in option description '{}'.",
                 desc.slice_from(last_end), desc)
        }
        try!(self.add_desc(short, long, arg))
        self.parse_default(full_desc)
    }

    fn parse_default(&mut self, desc: &str) -> Result<(), String> {
        let rdefault = regex!(r"(?i)\[default:(?P<val>[^]]*)\]");
        let defval =
            match rdefault.captures(desc) {
                None => return Ok(()),
                Some(c) => c.name("val").trim(),
            };
        let last_desc_key =
            match self.last_desc_added {
                Some(ref key) => key.clone(),
                None => err!("Found default value '{}' in '{}' before first \
                              option description.", defval, desc),
            };
        let desc = self.lookup(&last_desc_key)
                   .expect(format!("BUG: last opt desc key ('{}') is invalid.",
                                   last_desc_key).as_slice());
        desc.atype = Flag(Default(defval.to_string()));
        Ok(())
    }

    fn add_desc(&mut self, short: String, long: String, arg: ArgDesc)
               -> Result<(), String> {
        assert!(!short.is_empty() || !long.is_empty());
        if !short.is_empty() && short.len() != 2 {
            err!("Short flag '{}' is not of the form '-x'.", short);
        }
        if self.lookup(&short).is_some() {
            err!("Short flag '{}' is defined more than once.", short);
        }
        if self.lookup(&long).is_some() {
            err!("Long flag '{}' is defined more than once.", long);
        }
        let mut canonical = long;
        if canonical.is_empty() {
            canonical = short.clone();
        }
        if !short.is_empty() {
            self.short_to.insert(short, canonical.clone());
        }
        self.last_desc_added = Some(canonical.clone());
        self.descs.insert(canonical.clone(), AtomDesc {
            name: canonical,
            repeats: false, // not known yet; discovered in usage patterns
            atype: Flag(arg), // arg desc may change if default value is found
        });
        Ok(())
    }

    fn lookup<'a>(&'a mut self, flag: &String) -> Option<&'a mut AtomDesc> {
        let key =
            if is_short(flag.as_slice()) {
                match self.short_to.find(flag) {
                    Some(long) => long,
                    None => return None,
                }
            } else {
                flag
            };
        self.descs.find_mut(key)
    }
}

fn err_if_invalid_arg(s: &str) -> Result<(), String> {
    if !is_arg(s) {
        err!("Argument '{}' is not of the form ARG or <arg>.", s)
    } else {
        Ok(())
    }
}

fn is_short(s: &str) -> bool { return regex!(r"^-[^-]+$").is_match(s) }
fn is_long(s: &str) -> bool { return regex!(r"^--\S+$").is_match(s) }
fn is_arg(s: &str) -> bool { return regex!(r"^(\p{Lu}+|<[^>]+>)$").is_match(s) }
fn is_cmd(s: &str) -> bool { return regex!(r"^(-|--|[^-]\S+)$").is_match(s) }
