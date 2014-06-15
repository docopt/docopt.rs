use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::str::{MaybeOwned, Owned, Slice};
use regex;
use regex::Regex;

pub struct Docopt {
    program: String,
    usages: Vec<Pattern>,
    descs: AtomOptions,
    last_atom_added: Option<Atom>, // context for [default: ...]
}

type AtomOptions = HashMap<Atom, Rc<RefCell<Options>>>;

#[deriving(Show)]
enum Pattern {
    Alternates(Vec<Pattern>),
    Sequence(Vec<Pattern>),
    Repeat(Box<Pattern>),
    Optional(Box<Pattern>),
    Options,
    Atom(Atom),
}

#[deriving(PartialEq, Eq, Hash, Clone, Show)]
enum Atom {
    Short(char),
    Long(String),
    Command(String),
    Positional(String),
}

#[deriving(Clone, Show)]
struct Options {
    /// Set to true if this atom is ever repeated in any context.
    /// For positional arguments, non-argument flags and commands, repetition 
    /// means that they become countable.
    /// For flags with arguments, repetition means multiple distinct values
    /// can be specified (and are represented as a Vec).
    repeats: bool,

    /// This specifies whether this atom has any arguments.
    /// For commands and positional arguments, this is always Zero.
    /// Flags can have zero or one argument, with an optionally default value.
    arg: Argument,
}

#[deriving(Clone, Show, PartialEq)]
enum Argument {
    Zero,
    One,
    // Default implies One
    Default(String),
}

impl Pattern {
    fn flatten(self) -> Pattern {
        match self {
            Alternates(mut ps) => {
                // assert!(!ps.is_empty()); 
                if ps.len() == 1 {
                    ps.pop().unwrap().flatten()
                } else {
                    Alternates(ps.move_iter().map(|p| p.flatten()).collect())
                }
            }
            Sequence(mut ps) => {
                // assert!(!ps.is_empty()); 
                if ps.len() == 1 {
                    ps.pop().unwrap().flatten()
                } else {
                    Sequence(ps.move_iter().map(|p| p.flatten()).collect())
                }
            }
            Repeat(p) => Repeat(box p.flatten()),
            Optional(p) => Optional(box p.flatten()),
            Options => Options,
            Atom(name) => Atom(name),
        }
    }
}

impl Options {
    fn new_rc(rep: bool, arg: Argument) -> Rc<RefCell<Options>> {
        Rc::new(RefCell::new(Options { repeats: rep, arg: arg }))
    }
}

impl Argument {
    fn has_arg(&self) -> bool {
        match *self {
            Zero => false,
            One | Default(_) => true,
        }
    }
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
            last_atom_added: None,
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
        for pat in pats.captures_iter(caps.name("pats")) {
            let pattern = try!(PatParser::new(self, pat.at(1)).pattern());
            self.usages.push(pattern);
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
        let mut has_arg = false;
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
            if !flags.name("arg").is_empty() {
                try!(err_if_invalid_arg(flags.name("arg")));
                has_arg = true; // may be changed to default later
            }
        }
        // Make sure that we consumed everything. If there are leftovers,
        // then there is some malformed description. Alert the user.
        assert!(last_end <= desc.len());
        if last_end < desc.len() {
            err!("Extraneous text '{}' in option description '{}'.",
                 desc.slice_from(last_end), desc)
        }
        try!(self.add_desc(short.as_slice(), long.as_slice(), has_arg))
        // Looking for default in this line must come after adding the
        // description, otherwise `parse_default` won't know which option
        // to assign it to.
        self.parse_default(full_desc)
    }

    fn parse_default(&mut self, desc: &str) -> Result<(), String> {
        let rdefault = regex!(r"(?i)\[default:(?P<val>[^]]*)\]");
        let defval =
            match rdefault.captures(desc) {
                None => return Ok(()),
                Some(c) => c.name("val").trim(),
            };
        let last_atom =
            match self.last_atom_added {
                None => err!("Found default value '{}' in '{}' before first \
                              option description.", defval, desc),
                Some(ref atom) => atom,
            };
        let mut opts =
            self.descs.find(last_atom)
            .expect(format!("BUG: last opt desc key ('{}') is invalid.",
                            last_atom).as_slice())
            .borrow_mut();
        match opts.arg {
            One => {}, // OK
            Zero =>
                err!("Cannot assign default value '{}' to flag '{}' \
                      that has no arguments.", defval, last_atom),
            Default(ref curval) =>
                err!("Flag '{}' already has a default value \
                      of '{}' (second default value: '{}').",
                     last_atom, curval, defval),
        }
        opts.arg = Default(defval.to_string());
        Ok(())
    }

    fn add_desc(&mut self, short: &str, long: &str, has_arg: bool)
               -> Result<(), String> {
        assert!(!short.is_empty() || !long.is_empty());
        if !short.is_empty() && short.char_len() != 2 {
            err!("Short flag '{}' is not of the form '-x'.", short);
        }
        let opts = Options::new_rc(false, if has_arg { One } else { Zero });

        if !short.is_empty() {
            self.descs.insert(as_short(short), opts.clone());
            if long.is_empty() {
                self.last_atom_added = Some(as_short(short));
            }
        }
        if !long.is_empty() {
            self.descs.insert(as_long(long), opts);
            self.last_atom_added = Some(as_long(long))
        }
        Ok(())
    }
}

struct PatParser<'a> {
    dopt: &'a mut Docopt,
    tokens: Vec<String>, // used while parsing a single usage pattern
    curi: uint, // ^^ index into pattern chars
    seen: HashSet<Atom>, // atoms seen, used to track whether repeated
}

impl<'a> PatParser<'a> {
    fn new(dopt: &'a mut Docopt, pat: &str) -> PatParser<'a> {
        // Normalize `[xyz]` -> `[ xyz ]` so that grouping operators are
        // tokenized easily. Don't worry about introducing extra spaces.
        let rpat = regex!(r"\.\.\.|\[|\]|\(|\)|\|");
        let pat = rpat.replace_all(pat.trim(), " $0 ");
        PatParser {
            dopt: dopt,
            tokens: pat.as_slice().words().map(|s| s.to_string()).collect(),
            curi: 0,
            seen: HashSet::new(),
        }
    }

    fn pattern(&mut self) -> Result<Pattern, String> {
        let mut alts = vec!();
        while !self.is_eof() {
            match self.cur() {
                "]" | ")" => { self.next(); break },
                _ => alts.push(try!(self.alternate())),
            }
        }
        Ok(Alternates(alts).flatten())
    }

    fn alternate(&mut self) -> Result<Pattern, String> {
        let mut seq = vec!();
        while !self.is_eof() {
            match self.cur() {
                // "..." => { 
                    // err!("'...' must appear directly after a group, argument, \ 
                          // flag or command.") 
                // } 
                "-" | "--" => {
                    // As per specification, `-` and `--` by themselves are
                    // just commands that should be interpreted conventionally.
                    seq.push(try!(self.command()));
                    self.next();
                }
                "|" => {
                    if seq.is_empty() {
                        err!("Unexpected '|'. Not in form 'a | b | c'.")
                    }
                    try!(self.next_noeof("pattern"));
                    return Ok(Sequence(seq))
                }
                "]" | ")" => {
                    // if alt.pats.is_empty() { 
                        // err!("Unexpected '{}'. Empty groups are not allowed.", 
                             // self.cur()) 
                    // } 
                    return Ok(Sequence(seq))
                }
                "[" => {
                    // Check for special '[options]' shortcut.
                    if self.peekis("options") && self.peekpeekis("]") {
                        seq.push(Options);
                        self.next(); // cur == options
                        self.next(); // cur == ]
                        self.next();
                        continue
                    }
                    seq.push(try!(self.group(|p| Optional(box p), "]")));
                }
                "(" => {
                    seq.push(try!(self.group(|p| Sequence(vec!(p)), ")")));
                }
                _ => {
                    if is_short(self.cur()) {
                        println!("Short: {}", self.cur());
                        self.next();
                    } else if is_long(self.cur()) {
                        seq.push(try!(self.flag_long()));
                        // println!("Long: {}", self.cur()); 
                        // let rep = try!(self.flag_long()); 
                        // alt.pats.push(rep); 
                    } else if is_arg(self.cur()) {
                        // These are always positional.
                        // Arguments for -s and --short are picked up
                        // when parsing flags.
                        seq.push(try!(self.positional()));
                    } else if is_cmd(self.cur()) {
                        seq.push(try!(self.command()));
                    } else {
                        err!("Unknown token type '{}'.", self.cur())
                    }
                }
            }
        }
        Ok(Sequence(seq))
    }

    fn flag_long(&mut self) -> Result<Pattern, String> {
        let (atom, arg) = try!(parse_long_equal(self.cur()));
        if self.dopt.descs.contains_key(&atom) {
            // Options already exist for this atom, so we must check to make
            // sure things are consistent.
            let opts = self.dopt.descs.get(&atom).borrow().clone();
            if arg.has_arg() && !opts.arg.has_arg() {
                // Found `=` in usage, but previous usage of this flag
                // didn't specify an argument.
                err!("Flag '{}' does not take any arguments.", atom)
            } else if !arg.has_arg() && opts.arg.has_arg() {
                // Didn't find any `=` in usage for this flag, but previous
                // usage of this flag specifies an argument.
                // So look for `--flag ARG`
                try!(self.next_noeof(format!(
                     "Expected argument for flag '{}' but found EOF.",
                     atom).as_slice()));
                if !is_arg(self.cur()) {
                    err!("Expected argument for flag '{}', but found \
                          malformed argument '{}'.", atom, self.cur())
                }
                // We don't care about the value of `arg` since options
                // already exist. (In which case, the argument value can never
                // change.)
            }
        }
        self.next();
        let pat = self.maybe_repeat(Atom(atom.clone()));
        self.update_options(arg, &pat);
        Ok(pat)
    }

    fn command(&mut self) -> Result<Pattern, String> {
        let atom = Atom(as_cmd(self.cur()));
        self.next();
        let pat = self.maybe_repeat(atom);
        self.update_options(Zero, &pat);
        Ok(pat)
    }

    fn positional(&mut self) -> Result<Pattern, String> {
        let atom = Atom(as_arg(self.cur()));
        self.next();
        let pat = self.maybe_repeat(atom);
        self.update_options(Zero, &pat);
        Ok(pat)
    }

    fn update_options(&mut self, arg: Argument, atom_or_repeat: &Pattern) {
        let (atom, rep) =
            match atom_or_repeat {
                &Atom(ref atom) => (atom, false),
                &Repeat(box Atom(ref atom)) => (atom, true),
                _ => fail!("BUG: unexpected pattern: {}", atom_or_repeat),
            };
        let seen = self.seen.contains(atom);
        if !self.dopt.descs.contains_key(atom) {
            let opts = Options::new_rc(rep, arg);
            self.dopt.descs.insert(atom.clone(), opts);
        } else {
            let mut opts = self.dopt.descs.get(atom).borrow_mut();
            opts.repeats = opts.repeats || rep || seen;
        }
        self.seen.insert(atom.clone());
    }

    fn group(&mut self, mk: |Pattern| -> Pattern, end: &str)
            -> Result<Pattern, String> {
        try!(self.next_noeof("pattern"));
        let pat = try!(self.pattern());
        if !self.previs(end) {
            err!("Expected '{}' but got '{}'.", end, self.prev())
        }
        Ok(self.maybe_repeat(mk(pat)))
    }

    fn maybe_repeat(&mut self, pat: Pattern) -> Pattern {
        if self.curis("...") {
            self.next();
            Repeat(box pat)
        } else {
            pat
        }
    }

    fn is_eof(&self) -> bool {
        self.curi == self.tokens.len()
    }
    fn next(&mut self) {
        if self.curi == self.tokens.len() {
            return
        }
        self.curi += 1;
    }
    fn next_noeof(&mut self, expected: &str) -> Result<(), String> {
        self.next();
        if self.curi == self.tokens.len() {
            err!("Expected '{}' but reached end of usage pattern.", expected)
        }
        Ok(())
    }
    fn cur<'r>(&'r self) -> &'r str {
        self.tokens.get(self.curi).as_slice()
    }
    fn prev<'r>(&'r self) -> &'r str {
        self.tokens.get(self.curi - 1).as_slice()
    }
    fn cur_or_eof<'r>(&'r self) -> MaybeOwned<'r> {
        if self.is_eof() {
            Owned("EOF".to_string())
        } else {
            Slice(self.cur())
        }
    }
    fn curis(&self, is: &str) -> bool {
        !self.is_eof() && self.cur() == is
    }
    fn previs(&self, is: &str) -> bool {
        self.curi > 0 && self.tokens.get(self.curi - 1).as_slice() == is
    }
    fn peekis(&self, is: &str) -> bool {
        if self.curi + 1 >= self.tokens.len() {
            return false
        }
        self.tokens.get(self.curi + 1).as_slice() == is
    }
    fn peekpeekis(&self, is: &str) -> bool {
        if self.curi + 2 >= self.tokens.len() {
            return false
        }
        self.tokens.get(self.curi + 2).as_slice() == is
    }
}

fn err_if_invalid_arg(s: &str) -> Result<(), String> {
    if !is_arg(s) {
        err!("Argument '{}' is not of the form ARG or <arg>.", s)
    } else {
        Ok(())
    }
}

// Tries to parse a long flag of the form '--flag[=arg]' and returns a tuple
// with the flag atom and whether there is an argument or not.
// If '=arg' exists and 'arg' isn't a valid argument, an error is returned.
fn parse_long_equal(flag: &str) -> Result<(Atom, Argument), String> {
    let rarg = regex!("^(?P<name>[^=]+)=(?P<arg>.+)$");
    match rarg.captures(flag) {
        None => Ok((as_long(flag), Zero)),
        Some(cap) => {
            let arg = cap.name("arg").to_string();
            if !is_arg(arg.as_slice()) {
                err!("Argument '{}' for flag '{}' is not in the \
                      form ARG or <arg>.", flag, arg)
            }
            Ok((as_long(cap.name("name")), One))
        }
    }
}

fn as_short(s: &str) -> Atom {
    Short(s.as_slice().char_at(1))
}
fn as_long(s: &str) -> Atom {
    Long(s.as_slice().slice_from(2).to_string())
}
fn as_arg(s: &str) -> Atom {
    if s.starts_with("<") && s.ends_with(">") {
        Positional(s.slice(1, s.len()-1).to_string())
    } else {
        Positional(s.to_string())
    }
}
fn as_cmd(s: &str) -> Atom {
    Command(s.to_string())
}

fn is_short(s: &str) -> bool { return regex!(r"^-[^-]+$").is_match(s) }
fn is_long(s: &str) -> bool { return regex!(r"^--\S+$").is_match(s) }
fn is_arg(s: &str) -> bool { return regex!(r"^(\p{Lu}+|<[^>]+>)$").is_match(s) }
fn is_cmd(s: &str) -> bool { return regex!(r"^(-|--|[^-]\S*)$").is_match(s) }

impl fmt::Show for Docopt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        try!(writeln!(f, "====="))
        try!(writeln!(f, "Program: {}", self.program))
        try!(writeln!(f, "Option descriptions:"))
        for (k, v) in self.descs.iter() {
            try!(writeln!(f, "  '{}' => {}", k, v.borrow()))
        }
        try!(writeln!(f, "Usages:"))
        for pat in self.usages.iter() {
            try!(writeln!(f, "  {}", pat))
        }
        writeln!(f, "=====")
    }
}
