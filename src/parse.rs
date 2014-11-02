// I am overall pretty displeased with the quality of code in this module.
// I wrote it while simultaneously trying to build a mental model of Docopt's
// specification (hint: one does not exist in written form). As a result, there
// is a lot of coupling and some duplication.
//
// Some things that I think are good about the following code:
//
//   - The representation of a "usage pattern." I think it is a minimal
//     representation of a pattern's syntax. (One possible tweak:
//     `Optional<Vec<Pattern>>` -> `Optional<Box<Pattern>>`.)
//   - Some disciplined use of regexes. I use a pretty basic state machine
//     for parsing patterns, but for teasing out the patterns and options
//     from the Docopt string and for picking out flags with arguments, I
//     think regexes aren't too bad. There may be one or two scary ones though.
//   - The core matching algorithm is reasonably simple and concise, but I
//     think writing down some contracts will help me figure out how to make
//     the code clearer.
//
// Some bad things:
//
//   - I tried several times to split some of the pieces in this module into
//     separate modules. I could find no clear separation. This suggests that
//     there is too much coupling between parsing components. I'm not convinced
//     that the coupling is necessary.
//   - The parsers for patterns and argv share some structure. There may be
//     an easy abstraction waiting there.
//   - It is not efficient in the slightest. I tried to be conservative with
//     copying strings, but I think I failed. (It may not be worthwhile to fix
//     this if it makes the code more awkward. Docopt does not need to be
//     efficient.)
//
// Some things to do immediately:
//
//   - Document representation and invariants.
//   - Less important: write contracts for functions.
//
// Long term:
//
//   - Write a specification for Docopt.

use std::collections::{HashMap, HashSet};
use std::collections::hashmap::{Vacant, Occupied};
use std::fmt;
use regex;
use regex::Regex;

use {Value, Switch, Counted, Plain, List};
use synonym::SynonymMap;

macro_rules! err(
    ($($arg:tt)*) => (return Err(format!($($arg)*)))
)

#[deriving(Clone)]
pub struct Parser {
    pub program: String,
    pub full_doc: String,
    pub usage: String,
    pub descs: SynonymMap<Atom, Options>,
    usages: Vec<Pattern>,
    last_atom_added: Option<Atom>, // context for [default: ...]
}

impl Parser {
    pub fn new(doc: &str) -> Result<Parser, String> {
        let mut d = Parser {
            program: "".to_string(),
            full_doc: doc.to_string(),
            usage: "".to_string(),
            usages: vec!(),
            descs: SynonymMap::new(),
            last_atom_added: None,
        };
        try!(d.parse(doc));
        Ok(d)
    }

    pub fn matches(&self, argv: &Argv) -> Option<SynonymMap<String, Value>> {
        for usage in self.usages.iter() {
            match Matcher::matches(argv, usage) {
                None => continue,
                Some(vals) => return Some(vals),
            }
        }
        None
    }

    pub fn parse_argv<'a>(&'a self, argv: Vec<String>, options_first: bool)
                         -> Result<Argv<'a>, String> {
        Argv::new(self, argv, options_first)
    }
}

impl Parser {
    fn options_atoms(&self) -> Vec<Atom> {
        let mut atoms = vec!();
        for (atom, _) in self.descs.iter().filter(|&(_, opts)| opts.is_desc) {
            atoms.push(atom.clone());
        }
        atoms
    }

    fn has_arg(&self, atom: &Atom) -> bool {
        match self.descs.find(atom) {
            None => false,
            Some(opts) => opts.arg.has_arg(),
        }
    }

    fn parse(&mut self, doc: &str) -> Result<(), String> {
        let musage = regex!(r"(?is)usage:\s*(?P<prog>\S+)(?P<pats>.*?)(?:$|\n\s*\n)");
        let caps = match musage.captures(doc) {
            None => err!("Could not find usage patterns in doc string."),
            Some(caps) => caps,
        };
        if caps.name("prog").is_empty() {
            err!("Could not find program name in doc string.")
        }
        self.program = caps.name("prog").to_string();
        self.usage = caps.at(0).to_string();

        // Before we parse the usage patterns, we look for option descriptions.
        // We do this because the information in option descriptions can be
        // used to resolve ambiguities in usage patterns (i.e., whether
        // `--flag ARG` is a flag with an argument or not).
        //
        // From the docopt page, "every" line starting with a `-` or a `--`
        // is considered an option description. Instead, we restrict the lines
        // to any line *not* in the usage pattern section.
        //
        // *sigh* Apparently the above is not true. The official test suite
        // includes `Options: -a ...`, which means some lines not beginning
        // with `-` can actually have options.
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
        let mut last = caps.name("prog");
        for pat in pats.captures_iter(caps.name("pats")) {
            let pattern = try!(PatParser::new(self, pat.at(1)).parse());
            self.usages.push(pattern);
            last = pat.at(2);
        }
        if last == caps.name("prog") {
            // The last "Usage: ..." is an empty pattern.
            let pattern = try!(PatParser::new(self, "").parse());
            self.usages.push(pattern);
        }
        Ok(())
    }

    fn parse_desc(&mut self, full_desc: &str) -> Result<(), String> {
        let desc = regex!(r"(?i)^\s*options:\s*").replace(full_desc.trim(), "");
        let desc = desc.as_slice();
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
                let arg = flags.name("arg");
                if !Atom::is_arg(arg) {
                    err!("Argument '{}' is not of the form ARG or <arg>.", arg)
                }
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
        let opts =
            self.descs.find_mut(last_atom)
            .expect(format!("BUG: last opt desc key ('{}') is invalid.",
                            last_atom).as_slice());
        match opts.arg {
            One(None) => {}, // OK
            Zero =>
                err!("Cannot assign default value '{}' to flag '{}' \
                      that has no arguments.", defval, last_atom),
            One(Some(ref curval)) =>
                err!("Flag '{}' already has a default value \
                      of '{}' (second default value: '{}').",
                     last_atom, curval, defval),
        }
        opts.arg = One(Some(defval.to_string()));
        Ok(())
    }

    fn add_desc(&mut self, short: &str, long: &str, has_arg: bool)
               -> Result<(), String> {
        assert!(!short.is_empty() || !long.is_empty());
        if !short.is_empty() && short.char_len() != 2 {
            // It looks like the reference implementation just ignores
            // these lines.
            return Ok(());
            // err!("Short flag '{}' is not of the form '-x'.", short);
        }
        let mut opts = Options::new(false, if has_arg { One(None) } else { Zero });
        opts.is_desc = true;

        if !short.is_empty() && !long.is_empty() {
            let (short, long) = (Atom::new(short), Atom::new(long));
            self.descs.insert(long.clone(), opts);
            self.descs.insert_synonym(short, long.clone());
            self.last_atom_added = Some(long);
        } else if !short.is_empty() {
            let short = Atom::new(short);
            self.descs.insert(short.clone(), opts);
            self.last_atom_added = Some(short);
        } else if !long.is_empty() {
            let long = Atom::new(long);
            self.descs.insert(long.clone(), opts);
            self.last_atom_added = Some(long);
        }
        Ok(())
    }
}

impl fmt::Show for Parser {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        fn sorted<T: Ord>(mut xs: Vec<T>) -> Vec<T> {
            xs.sort(); xs
        }

        try!(writeln!(f, "====="))
        try!(writeln!(f, "Program: {}", self.program))

        try!(writeln!(f, "Option descriptions:"))
        let keys = sorted(self.descs.keys().collect());
        for &k in keys.iter() {
            try!(writeln!(f, "  '{}' => {}", k, self.descs.get(k)))
        }

        try!(writeln!(f, "Synonyms:"))
        let keys: Vec<(&Atom, &Atom)> = sorted(self.descs.synonyms().collect());
        for &(from, to) in keys.iter() {
            try!(writeln!(f, "  {} => {}", from, to))
        }

        try!(writeln!(f, "Usages:"))
        for pat in self.usages.iter() {
            try!(writeln!(f, "  {}", pat))
        }
        writeln!(f, "=====")
    }
}

struct PatParser<'a> {
    dopt: &'a mut Parser,
    tokens: Vec<String>, // used while parsing a single usage pattern
    curi: uint, // ^^ index into pattern chars
    expecting: Vec<char>, // stack of expected ']' or ')'
}

impl<'a> PatParser<'a> {
    fn new(dopt: &'a mut Parser, pat: &str) -> PatParser<'a> {
        PatParser {
            dopt: dopt,
            tokens: pattern_tokens(pat),
            curi: 0,
            expecting: vec!(),
        }
    }

    fn parse(&mut self) -> Result<Pattern, String> {
        // let mut seen = HashSet::new();
        let mut p = try!(self.pattern());
        match self.expecting.pop() {
            None => {},
            Some(c) => err!("Unclosed group. Expected '{}'.", c),
        }
        p.add_options_shortcut(self.dopt);
        p.tag_repeats(&mut self.dopt.descs);
        Ok(p)
    }

    fn pattern(&mut self) -> Result<Pattern, String> {
        let mut alts = vec!();
        let mut seq = vec!();
        while !self.is_eof() {
            match self.cur() {
                "..." => {
                    err!("'...' must appear directly after a group, argument, \
                          flag or command.")
                }
                "-" | "--" => {
                    // As per specification, `-` and `--` by themselves are
                    // just commands that should be interpreted conventionally.
                    seq.push(try!(self.command()));
                }
                "|" => {
                    if seq.is_empty() {
                        err!("Unexpected '|'. Not in form 'a | b | c'.")
                    }
                    try!(self.next_noeof("pattern"));
                    alts.push(Sequence(seq));
                    seq = vec!();
                }
                "]" | ")" => {
                    if seq.is_empty() {
                        err!("Unexpected '{}'. Empty groups are not allowed.",
                             self.cur())
                    }
                    match self.expecting.pop() {
                        None => err!("Unexpected '{}'. No open bracket found.",
                                     self.cur()),
                        Some(c) => {
                            if c != self.cur().char_at(0) {
                                err!("Expected '{}' but got '{}'.",
                                     c, self.cur())
                            }
                        }
                    }
                    let mk = if self.cur() == "]" { Optional } else { Sequence };
                    self.next();
                    return
                        if alts.is_empty() {
                            Ok(mk(seq))
                        } else {
                            alts.push(Sequence(seq));
                            Ok(mk(vec!(Alternates(alts))))
                        }
                }
                "[" => {
                    // Check for special '[options]' shortcut.
                    if self.atis(1, "options") && self.atis(2, "]") {
                        // let atoms = self.dopt.options_atoms();
                        // let opts = Optional(atoms.into_iter().map(Atom).collect());
                        self.next(); // cur == options
                        self.next(); // cur == ]
                        self.next();
                        // seq.push(self.maybe_repeat(opts));
                        seq.push(self.maybe_repeat(Optional(vec!())));
                        continue
                    }
                    self.expecting.push(']');
                    seq.push(try!(self.group()));
                }
                "(" => {
                    self.expecting.push(')');
                    seq.push(try!(self.group()));
                }
                _ => {
                    if Atom::is_short(self.cur()) {
                        seq.extend(try!(self.flag_short()).into_iter());
                    } else if Atom::is_long(self.cur()) {
                        seq.push(try!(self.flag_long()));
                    } else if Atom::is_arg(self.cur()) {
                        // These are always positional.
                        // Arguments for -s and --short are picked up
                        // when parsing flags.
                        seq.push(try!(self.positional()));
                    } else if Atom::is_cmd(self.cur()) {
                        seq.push(try!(self.command()));
                    } else {
                        err!("Unknown token type '{}'.", self.cur())
                    }
                }
            }
        }
        if alts.is_empty() {
            Ok(Sequence(seq))
        } else {
            alts.push(Sequence(seq));
            Ok(Alternates(alts))
        }
    }

    fn flag_short(&mut self) -> Result<Vec<Pattern>, String> {
        let mut seq = vec!();
        let stacked: String = self.cur().slice_from(1).to_string();
        for (i, c) in stacked.as_slice().chars().enumerate() {
            let atom = self.dopt.descs.resolve(&Short(c));
            seq.push(PatAtom(atom.clone()));

            // The only way for a short option to have an argument is if
            // it's specified in an option description.
            if !self.dopt.has_arg(&atom) {
                self.add_atom_ifnotexists(Zero, &atom);
            } else {
                // At this point, the flag MUST have an argument. Therefore,
                // we interpret the "rest" of the characters as the argument.
                // If the "rest" is empty, then we peek to find and make sure
                // there is an argument.
                let rest = stacked.as_slice().slice_from(i+1);
                if rest.is_empty() {
                    try!(self.next_flag_arg(&atom));
                } else {
                    try!(self.errif_invalid_flag_arg(&atom, rest));
                }
                // We either error'd or consumed the rest of the short stack as
                // an argument.
                break
            }
        }
        self.next();
        // This is a little weird. We've got to manually look for a repeat
        // operator right after the stack, and then apply it to each short
        // flag we generated.
        // If "sequences" never altered semantics, then we could just use that
        // here to group a short stack.
        if self.atis(0, "...") {
            self.next();
            seq = seq.into_iter().map(|p| Repeat(box p)).collect();
        }
        Ok(seq)
    }

    fn flag_long(&mut self) -> Result<Pattern, String> {
        let (atom, arg) = try!(parse_long_equal(self.cur()));
        let atom = self.dopt.descs.resolve(&atom);
        if self.dopt.descs.contains_key(&atom) {
            // Options already exist for this atom, so we must check to make
            // sure things are consistent.
            let has_arg = self.dopt.has_arg(&atom);
            if arg.has_arg() && !has_arg {
                // Found `=` in usage, but previous usage of this flag
                // didn't specify an argument.
                err!("Flag '{}' does not take any arguments.", atom)
            } else if !arg.has_arg() && has_arg {
                // Didn't find any `=` in usage for this flag, but previous
                // usage of this flag specifies an argument.
                // So look for `--flag ARG`
                try!(self.next_flag_arg(&atom));
                // We don't care about the value of `arg` since options
                // already exist. (In which case, the argument value can never
                // change.)
            }
        }
        self.add_atom_ifnotexists(arg, &atom);
        self.next();
        Ok(self.maybe_repeat(PatAtom(atom)))
    }

    fn next_flag_arg(&mut self, atom: &Atom) -> Result<(), String> {
        try!(self.next_noeof(format!(
             "argument for flag '{}'", atom).as_slice()));
        self.errif_invalid_flag_arg(atom, self.cur())
    }

    fn errif_invalid_flag_arg(&self, atom: &Atom, arg: &str)
                             -> Result<(), String> {
        if !Atom::is_arg(arg) {
            err!("Expected argument for flag '{}', but found \
                  malformed argument '{}'.", atom, arg)
        }
        Ok(())
    }

    fn command(&mut self) -> Result<Pattern, String> {
        let atom = Atom::new(self.cur());
        self.add_atom_ifnotexists(Zero, &atom);
        self.next();
        Ok(self.maybe_repeat(PatAtom(atom)))
    }

    fn positional(&mut self) -> Result<Pattern, String> {
        let atom = Atom::new(self.cur());
        self.add_atom_ifnotexists(Zero, &atom);
        self.next();
        Ok(self.maybe_repeat(PatAtom(atom)))
    }

    fn add_atom_ifnotexists(&mut self, arg: Argument, atom: &Atom) {
        if !self.dopt.descs.contains_key(atom) {
            let opts = Options::new(false, arg);
            self.dopt.descs.insert(atom.clone(), opts);
        }
    }

    fn group(&mut self)
            -> Result<Pattern, String> {
        try!(self.next_noeof("pattern"));
        let pat = try!(self.pattern());
        Ok(self.maybe_repeat(pat))
    }

    fn maybe_repeat(&mut self, pat: Pattern) -> Pattern {
        if self.atis(0, "...") {
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
            err!("Expected {} but reached end of usage pattern.", expected)
        }
        Ok(())
    }
    fn cur<'r>(&'r self) -> &'r str {
        self.tokens[self.curi].as_slice()
    }
    fn atis(&self, offset: int, is: &str) -> bool {
        let i = (self.curi as int) + offset;
        let iu = i as uint;
        i >= 0 && iu < self.tokens.len() && self.tokens[iu].as_slice() == is
    }
}

#[deriving(Clone, Show)]
enum Pattern {
    Alternates(Vec<Pattern>),
    Sequence(Vec<Pattern>),
    Optional(Vec<Pattern>),
    Repeat(Box<Pattern>),
    PatAtom(Atom),
}

#[deriving(PartialEq, Eq, Ord, Hash, Clone)]
pub enum Atom {
    Short(char),
    Long(String),
    Command(String),
    Positional(String),
}

#[deriving(Clone, Show)]
pub struct Options {
    /// Set to true if this atom is ever repeated in any context.
    /// For positional arguments, non-argument flags and commands, repetition
    /// means that they become countable.
    /// For flags with arguments, repetition means multiple distinct values
    /// can be specified (and are represented as a Vec).
    pub repeats: bool,

    /// This specifies whether this atom has any arguments.
    /// For commands and positional arguments, this is always Zero.
    /// Flags can have zero or one argument, with an optionally default value.
    pub arg: Argument,

    /// Whether it shows up in the "options description" second.
    pub is_desc: bool,
}

#[deriving(Clone, Show, PartialEq)]
pub enum Argument {
    Zero,
    One(Option<String>), // optional default value
}

impl Pattern {
    fn add_options_shortcut(&mut self, par: &Parser) {
        fn add(pat: &mut Pattern, all_atoms: &HashSet<Atom>, par: &Parser) {
            match pat {
                &Alternates(ref mut ps) | &Sequence(ref mut ps) => {
                    for p in ps.iter_mut() { add(p, all_atoms, par) }
                }
                &Repeat(box ref mut p) => add(p, all_atoms, par),
                &PatAtom(_) => {}
                &Optional(ref mut ps) => {
                    if !ps.is_empty() {
                        for p in ps.iter_mut() { add(p, all_atoms, par) }
                    } else {
                        for atom in par.options_atoms().into_iter() {
                            if !all_atoms.contains(&atom) {
                                ps.push(PatAtom(atom));
                            }
                        }
                    }
                }
            }
        }
        let all_atoms = self.all_atoms();
        add(self, &all_atoms, par);
    }

    fn all_atoms(&self) -> HashSet<Atom> {
        fn all_atoms(pat: &Pattern, set: &mut HashSet<Atom>) {
            match pat {
                &Alternates(ref ps) | &Sequence(ref ps) | &Optional(ref ps) => {
                    for p in ps.iter() { all_atoms(p, set) }
                }
                &Repeat(box ref p) => all_atoms(p, set),
                &PatAtom(ref a) => { set.insert(a.clone()); }
            }
        }
        let mut set = HashSet::new();
        all_atoms(self, &mut set);
        set
    }

    fn tag_repeats(&self, map: &mut SynonymMap<Atom, Options>) {
        fn dotag(p: &Pattern,
                 rep: bool,
                 map: &mut SynonymMap<Atom, Options>,
                 seen: &mut HashSet<Atom>) {
            match p {
                &Alternates(ref ps) => {
                    // This is a bit tricky. Basically, we don't want the
                    // existence of an item in mutually exclusive alternations
                    // to affect whether it repeats or not.
                    // However, we still need to record seeing each item in
                    // each alternation.
                    let fresh = seen.clone();
                    for p in ps.iter() {
                        let mut isolated = fresh.clone();
                        dotag(p, rep, map, &mut isolated);
                        for a in isolated.into_iter() {
                            seen.insert(a);
                        }
                    }
                }
                &Sequence(ref ps) => {
                    for p in ps.iter() {
                        dotag(p, rep, map, seen)
                    }
                }
                &Optional(ref ps) => {
                    for p in ps.iter() {
                        dotag(p, rep, map, seen)
                    }
                }
                &Repeat(box ref p) => dotag(p, true, map, seen),
                &PatAtom(ref atom) => {
                    let opt = map.find_mut(atom).expect("bug: no atom found");
                    opt.repeats = opt.repeats || rep || seen.contains(atom);
                    seen.insert(atom.clone());
                }
            }
        }
        let mut seen = HashSet::new();
        dotag(self, false, map, &mut seen);
    }
}

impl Atom {
    pub fn new(s: &str) -> Atom {
        if Atom::is_short(s) {
            Short(s.as_slice().char_at(1))
        } else if Atom::is_long(s) {
            Long(s.as_slice().slice_from(2).to_string())
        } else if Atom::is_arg(s) {
            if s.starts_with("<") && s.ends_with(">") {
                Positional(s.slice(1, s.len()-1).to_string())
            } else {
                Positional(s.to_string())
            }
        } else if Atom::is_cmd(s) {
            Command(s.to_string())
        } else {
            panic!("Unknown atom string: '{}'", s)
        }
    }

    fn is_short(s: &str) -> bool { return regex!(r"^-[^-]+$").is_match(s) }
    fn is_long(s: &str) -> bool {
        return regex!(r"^--\S+(?:<[^>]+>)?$").is_match(s)
    }
    fn is_arg(s: &str) -> bool {
        return regex!(r"^(\p{Lu}+|<[^>]+>)$").is_match(s)
    }
    fn is_cmd(s: &str) -> bool {
        return regex!(r"^(-|--|[^-]\S*)$").is_match(s)
    }

    // Assigns an integer to each variant of Atom. (For easier sorting.)
    fn type_as_uint(&self) -> uint {
        match self {
            &Short(_) => 0,
            &Long(_) => 1,
            &Command(_) => 2,
            &Positional(_) => 3,
        }
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Atom) -> Option<Ordering> {
        match (self, other) {
            (&Short(c1), &Short(c2)) => c1.partial_cmp(&c2),
            (&Long(ref s1), &Long(ref s2)) => s1.partial_cmp(s2),
            (&Command(ref s1), &Command(ref s2)) => s1.partial_cmp(s2),
            (&Positional(ref s1), &Positional(ref s2)) => s1.partial_cmp(s2),
            (a1, a2) => a1.type_as_uint().partial_cmp(&a2.type_as_uint()),
        }
    }
}

impl fmt::Show for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Short(c) => write!(f, "-{}", c),
            &Long(ref s) => write!(f, "--{}", s),
            &Command(ref s) => write!(f, "{}", s),
            &Positional(ref s) => {
                if s.as_slice().chars().all(|c| c.is_uppercase()) {
                    write!(f, "{}", s)
                } else {
                    write!(f, "<{}>", s)
                }
            }
        }
    }
}

impl Options {
    fn new(rep: bool, arg: Argument) -> Options {
        Options { repeats: rep, arg: arg, is_desc: false, }
    }
}

impl Argument {
    fn has_arg(&self) -> bool {
        match *self {
            Zero => false,
            One(_) => true,
        }
    }
}

#[doc(hidden)]
pub struct Argv<'a> {
    /// A representation of an argv string as an ordered list of tokens.
    /// This contains only positional arguments and commands.
    positional: Vec<ArgvToken>,
    /// Same as positional, but contains short and long flags.
    /// Each flag may have an argument string.
    flags: Vec<ArgvToken>,
    /// Counts the number of times each flag appears.
    counts: HashMap<Atom, uint>,

    // State for parser.
    dopt: &'a Parser,
    argv: Vec<String>,
    curi: uint,
    options_first: bool,
}

#[deriving(Show)]
struct ArgvToken {
    atom: Atom,
    arg: Option<String>,
}

impl<'a> Argv<'a> {
    fn new(dopt: &'a Parser, argv: Vec<String>, options_first: bool)
          -> Result<Argv<'a>, String> {
        let mut a = Argv {
            positional: vec!(),
            flags: vec!(),
            counts: HashMap::new(),
            dopt: dopt,
            argv: argv.iter().map(|s| s.to_string()).collect(),
            curi: 0,
            options_first: options_first,
        };
        try!(a.parse());
        for flag in a.flags.iter() {
            match a.counts.entry(flag.atom.clone()) {
                Vacant(v) => { v.set(1); }
                Occupied(mut v) => { *v.get_mut() += 1; }
            }
        }
        Ok(a)
    }

    fn parse(&mut self) -> Result<(), String> {
        let mut seen_double_dash = false;
        while self.curi < self.argv.len() {
            let do_flags =
                !seen_double_dash
                && (!self.options_first || self.positional.is_empty());

            if do_flags && Atom::is_short(self.cur()) {
                let stacked: String = self.cur().slice_from(1).to_string();
                for (i, c) in stacked.as_slice().chars().enumerate() {
                    let mut tok = ArgvToken {
                        atom: self.dopt.descs.resolve(&Short(c)),
                        arg: None,
                    };
                    if !self.dopt.descs.contains_key(&tok.atom) {
                        err!("Unknown flag: '{}'", &tok.atom)
                    }
                    if !self.dopt.has_arg(&tok.atom) {
                        self.flags.push(tok);
                    } else {
                        let rest = stacked.as_slice().slice_from(i+1);
                        tok.arg = Some(
                            if rest.is_empty() {
                                let arg = try!(self.next_arg(&tok.atom));
                                arg.to_string()
                            } else {
                                rest.to_string()
                            }
                        );
                        self.flags.push(tok);
                        // We've either produced an error or gobbled up the
                        // rest of these stacked short flags, so stop.
                        break
                    }
                }
            } else if do_flags && Atom::is_long(self.cur()) {
                let (atom, mut arg) = parse_long_equal_argv(self.cur());
                let atom = self.dopt.descs.resolve(&atom);
                if !self.dopt.descs.contains_key(&atom) {
                    err!("Unknown flag: '{}'", &atom)
                }
                if arg.is_some() && !self.dopt.has_arg(&atom) {
                    err!("Flag '{}' cannot have an argument, but found '{}'.",
                         &atom, &arg)
                } else if arg.is_none() && self.dopt.has_arg(&atom) {
                    try!(self.next_noeof(
                        format!("argument for flag '{}'", &atom).as_slice()));
                    arg = Some(self.cur().to_string());
                }
                self.flags.push(ArgvToken { atom: atom, arg: arg });
            } else {
                if self.cur() == "--" {
                    seen_double_dash = true;
                }
                let tok = self.as_command_or_arg(self.cur());
                self.positional.push(tok);
            }
            self.next()
        }
        Ok(())
    }

    fn as_command_or_arg(&self, s: &str) -> ArgvToken {
        let cmd = Command(s.to_string());
        let atom =
            if self.dopt.descs.contains_key(&cmd) {
                cmd
            } else {
                Positional(s.to_string())
            };
        ArgvToken { atom: atom, arg: None }
    }

    fn cur<'a>(&'a self) -> &'a str { self.at(0) }
    fn at<'a>(&'a self, i: int) -> &'a str {
        self.argv[(self.curi as int + i) as uint].as_slice()
    }
    fn next(&mut self) {
        if self.curi < self.argv.len() {
            self.curi += 1
        }
    }
    fn next_arg<'a>(&'a mut self, atom: &Atom) -> Result<&'a str, String> {
        let expected = format!("argument for flag '{}'", atom);
        try!(self.next_noeof(expected.as_slice()));
        Ok(self.cur())
    }
    fn next_noeof(&mut self, expected: &str) -> Result<(), String> {
        self.next();
        if self.curi == self.argv.len() {
            err!("Expected {} but reached end of arguments.", expected)
        }
        Ok(())
    }
}

impl<'a> fmt::Show for Argv<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        try!(writeln!(f, "Positional: {}", self.positional))
        try!(writeln!(f, "Flags: {}", self.flags))
        try!(writeln!(f, "Counts: {}", self.counts))
        Ok(())
    }
}

struct Matcher<'a, 'b:'a> {
    argv: &'a Argv<'b>,
}

#[deriving(Clone, PartialEq, Show)]
struct MState {
    argvi: uint, // index into Argv.positional
    counts: HashMap<Atom, uint>, // flags remaining for pattern consumption
    max_counts: HashMap<Atom, uint>, // optional flag appearances
    vals: HashMap<Atom, Value>,
}

impl MState {
    fn fill_value(&mut self, key: Atom, rep: bool, arg: Option<String>) -> bool {
        match (arg, rep) {
            (None, false) => {
                self.vals.insert(key, Switch(true));
            }
            (Some(arg), false) => {
                self.vals.insert(key, Plain(Some(arg)));
            }
            (None, true) => {
                match self.vals.entry(key.clone()) {
                    Vacant(v) => { v.set(Counted(1)); }
                    Occupied(mut v) => {
                        match v.get_mut() {
                            &Counted(ref mut c) => { *c += 1; }
                            _ => return false,
                        }
                    }
                }
            }
            (Some(arg), true) => {
                match self.vals.entry(key.clone()) {
                    Vacant(v) => { v.set(List(vec!(arg))); }
                    Occupied(mut v) => {
                        match v.get_mut() {
                            &List(ref mut vs) => vs.push(arg),
                            _ => return false,
                        }
                    }
                }
            }
        }
        true
    }

    fn add_value(&mut self, opts: &Options,
                 spec: &Atom, atom: &Atom, arg: &Option<String>) -> bool {
        assert!(opts.arg.has_arg() == arg.is_some(),
                "'{}' should have an argument but doesn't", atom);
        match atom {
            &Short(_) | &Long(_) => {
                self.fill_value(spec.clone(), opts.repeats, arg.clone())
            }
            &Positional(ref v) => {
                assert!(!opts.arg.has_arg());
                self.fill_value(spec.clone(), opts.repeats, Some(v.clone()))
            }
            &Command(_) => {
                assert!(!opts.arg.has_arg());
                self.fill_value(spec.clone(), opts.repeats, None)
            }
        }
    }

    fn use_flag(&mut self, flag: &Atom) -> bool {
        match self.max_counts.entry(flag.clone()) {
            Vacant(v) => { v.set(0); }
            Occupied(_) => {}
        }
        match self.counts.entry(flag.clone()) {
            Vacant(_) => { false }
            Occupied(mut v) => {
                let c = v.get_mut();
                if *c == 0 {
                    false
                } else {
                    *c -= 1;
                    true
                }
            }
        }
    }

    fn use_optional_flag(&mut self, flag: &Atom) {
        match self.max_counts.entry(flag.clone()) {
            Vacant(v) => { v.set(1); }
            Occupied(mut v) => { *v.get_mut() += 1; }
        }
    }

    fn match_cmd_or_posarg(&mut self, spec: &Atom, argv: &ArgvToken) -> bool {
        match (spec, &argv.atom) {
            (&Command(ref n1), &Command(ref n2)) if n1 == n2 => {
                self.argvi += 1;
                true
            }
            (&Positional(_), _) => {
                self.argvi += 1;
                true
            }
            _ => false,
        }
    }
}

impl<'a, 'b> Matcher<'a, 'b> {
    fn matches(argv: &'a Argv, pat: &Pattern) -> Option<SynonymMap<String, Value>> {
        let m = Matcher { argv: argv };
        let init = MState {
            argvi: 0,
            counts: argv.counts.clone(),
            max_counts: HashMap::new(),
            vals: HashMap::new(),
        };
        m.states(pat, &init)
            .into_iter()
            .filter(|s| m.state_consumed_all_argv(s))
            .filter(|s| m.state_has_valid_flags(s))
            .filter(|s| m.state_valid_num_flags(s))
            .collect::<Vec<MState>>()
            .remove(0)
            .map(|mut s| {
                m.add_flag_values(&mut s);
                m.add_default_values(&mut s);

                // Build a synonym map so that it's easier to look up values.
                let mut synmap: SynonymMap<String, Value> =
                    s.vals.into_iter().map(|(k, v)| (k.to_string(), v)).collect();
                for (from, to) in argv.dopt.descs.synonyms() {
                    let (from, to) = (from.to_string(), to.to_string());
                    if synmap.contains_key(&to) {
                        synmap.insert_synonym(from, to);
                    }
                }
                synmap
            })
    }

    fn token_from(&'a self, state: &MState) -> Option<&'a ArgvToken> {
        self.argv.positional.as_slice().get(state.argvi)
    }

    fn add_value(&self, state: &mut MState,
                  atom_spec: &Atom, atom: &Atom, arg: &Option<String>) -> bool {
        let opts = self.argv.dopt.descs.get(atom_spec);
        state.add_value(opts, atom_spec, atom, arg)
    }

    fn add_flag_values(&self, state: &mut MState) {
        for tok in self.argv.flags.iter() {
            self.add_value(state, &tok.atom, &tok.atom, &tok.arg);
        }
    }

    fn add_default_values(&self, state: &mut MState) {
        let vs = &mut state.vals;
        for (a, opts) in self.argv.dopt.descs.iter() {
            if vs.contains_key(a) {
                continue
            }
            let atom = a.clone();
            match (opts.repeats, &opts.arg) {
                (false, &Zero) => {
                    match a {
                        &Positional(_) => vs.insert(atom, Plain(None)),
                        _ => vs.insert(atom, Switch(false)),
                    };
                }
                (true, &Zero) => {
                    match a {
                        &Positional(_) => vs.insert(atom, List(vec!())),
                        _ => vs.insert(atom, Counted(0)),
                    };
                }
                (false, &One(None)) => { vs.insert(atom, Plain(None)); }
                (true, &One(None)) => { vs.insert(atom, List(vec!())); }
                (false, &One(Some(ref v))) => {
                    vs.insert(atom, Plain(Some(v.clone())));
                }
                (true, &One(Some(ref v))) => {
                    let words =
                        v.as_slice().words().map(|s| s.to_string()).collect();
                    vs.insert(atom, List(words));
                }
            }
        }
    }

    fn state_consumed_all_argv(&self, state: &MState) -> bool {
        self.argv.positional.len() == state.argvi
    }

    fn state_has_valid_flags(&self, state: &MState) -> bool {
        self.argv.counts.keys().all(|flag| state.max_counts.contains_key(flag))
    }

    fn state_valid_num_flags(&self, state: &MState) -> bool {
        state.counts.iter().all(
            |(flag, count)| count <= &state.max_counts[*flag])
    }

    fn states(&self, pat: &Pattern, init: &MState) -> Vec<MState> {
        match pat {
            &Alternates(ref ps) => {
                let mut alt_states = vec!();
                for p in ps.iter() {
                    alt_states.extend(self.states(p, init).into_iter());
                }
                alt_states
            }
            &Sequence(ref ps) => {
                let (mut states, mut next) = (vec!(), vec!());
                let mut iter = ps.iter();
                match iter.next() {
                    None => return vec!(init.clone()),
                    Some(p) => states.extend(self.states(p, init).into_iter()),
                }
                for p in iter {
                    for s in states.into_iter() {
                        next.extend(self.states(p, &s).into_iter());
                    }
                    states = vec!();
                    states.extend(next.into_iter());
                    next = vec!();
                }
                states
            }
            &Optional(ref ps) => {
                let mut base = init.clone();
                let mut noflags = vec!();
                for p in ps.iter() {
                    match p {
                        &PatAtom(ref a @ Short(_)) | &PatAtom(ref a @Long(_)) => {
                            let argv_count = self.argv.counts.find_copy(a).unwrap_or(0);
                            let max_count = base.max_counts.find_copy(a).unwrap_or(0);
                            if argv_count > max_count {
                                base.use_optional_flag(a);
                            }
                        }
                        other => {
                            noflags.push(other);
                        }
                    }
                }
                let mut states = vec!();
                self.all_option_states(&base, &mut states, noflags.as_slice());
                states
            }
            &Repeat(box ref p) => {
                let mut grouped_states = vec!(self.states(p, init));
                loop {
                    let mut nextss = vec!();
                    for s in grouped_states.last().unwrap().iter() {
                        nextss.extend(
                            self.states(p, s)
                                .into_iter()
                                .filter(|snext| snext != s));
                    }
                    if nextss.is_empty() {
                        break
                    }
                    grouped_states.push(nextss);
                }
                grouped_states
                    .into_iter()
                    .flat_map(|ss| ss.into_iter())
                    .collect::<Vec<MState>>()
            }
            &PatAtom(ref atom) => {
                let mut state = init.clone();
                match atom {
                    &Short(_) | &Long(_) => {
                        if !state.use_flag(atom) {
                            return vec!()
                        }
                    }
                    &Command(_) | &Positional(_) => {
                        let tok =
                            match self.token_from(init) {
                                None => return vec!(),
                                Some(tok) => tok,
                            };
                        if !state.match_cmd_or_posarg(atom, tok) {
                            return vec!()
                        }
                        if !self.add_value(&mut state, atom, &tok.atom, &tok.arg) {
                            return vec!()
                        }
                    }
                }
                vec!(state)
            }
        }
    }

    fn all_option_states(&self, base: &MState, states: &mut Vec<MState>,
                         pats: &[&Pattern]) {
        if pats.is_empty() {
            states.push(base.clone());
        } else {
            let (pat, rest) = (*pats.head().unwrap(), pats.tail());
            for s in self.states(pat, base).into_iter() {
                self.all_option_states(&s, states, rest);
            }
            // Order is important here! This must come after the loop above
            // because we prefer presence over absence. The first state wins.
            self.all_option_states(base, states, pats.tail());
        }
    }
}

// Tries to parse a long flag of the form '--flag[=arg]' and returns a tuple
// with the flag atom and whether there is an argument or not.
// If '=arg' exists and 'arg' isn't a valid argument, an error is returned.
fn parse_long_equal(flag: &str) -> Result<(Atom, Argument), String> {
    let long_equal = regex!("^(?P<name>[^=]+)=(?P<arg>.+)$");
    match long_equal.captures(flag) {
        None => Ok((Atom::new(flag), Zero)),
        Some(cap) => {
            let arg = cap.name("arg").to_string();
            if !Atom::is_arg(arg.as_slice()) {
                err!("Argument '{}' for flag '{}' is not in the \
                      form ARG or <arg>.", flag, arg)
            }
            Ok((Atom::new(cap.name("name")), One(None)))
        }
    }
}

fn parse_long_equal_argv(flag: &str) -> (Atom, Option<String>) {
    let long_equal = regex!("^(?P<name>[^=]+)=(?P<arg>.*)$");
    match long_equal.captures(flag) {
        None => (Atom::new(flag), None),
        Some(cap) =>
            (Atom::new(cap.name("name")), Some(cap.name("arg").to_string())),
    }
}

// Tokenizes a usage pattern.
// Beware: regex hack ahead. Tokenizes based on whitespace separated words.
// It first normalizes `[xyz]` -> `[ xyz ]` so that delimiters are tokens.
// Similarly for `...`, `(`, `)` and `|`.
// One hitch: `--flag=<arg spaces>` is allowed, so we use a regex to pick out
// words.
fn pattern_tokens(pat: &str) -> Vec<String> {
    let rpat = regex!(r"\.\.\.|\[|\]|\(|\)|\|");
    let rwords = regex!(r"--\S+?=<[^>]+>|<[^>]+>|\S+"); // alt order matters

    let pat = rpat.replace_all(pat.trim(), " $0 ");
    let mut words = vec!();
    for cap in rwords.captures_iter(pat.as_slice()) {
        words.push(cap.at(0).to_string());
    }
    words
}
