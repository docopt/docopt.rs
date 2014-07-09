//! Docopt for Rust. This implementation conforms to the
//! [official description of Docopt](http://docopt.org/) and
//! [passes its test suite](https://github.com/docopt/docopt/pull/201).
//!
//! This library is [on GitHub](https://github.com/BurntSushi/docopt.rs)
//! and is currently very experimental, particularly with respect to the
//! API and its use of macros. Feedback is **very** welcome.
//!
//! Fundamentally, Docopt is a command line argument parser. The detail that
//! distinguishes it from most parsers is that the parser is derived from the
//! usage string. Here's a simple example using the `docopt!` macro:
//!
//! ```rust
//! #![feature(phase)]
//! extern crate serialize;
//! #[phase(plugin)] extern crate docopt_macros;
//! extern crate docopt;
//!
//! use docopt::FlagParser;
//!
//! docopt!(Args, "
//! Usage: cp [-a] SOURCE DEST
//!        cp [-a] SOURCE... DIR
//! 
//! Options:
//!     -a, --archive  Copy everything.
//! ")
//!
//! fn main() {
//!     let argv = &["-a", "file1", "file2", "dest/"];
//!     let args: Args = FlagParser::parse_args(docopt::DEFAULT_CONFIG.clone(), argv);
//!     assert!(args.flag_archive);
//!     assert_eq!(args.arg_SOURCE, vec!["file1".to_string(), "file2".to_string()]);
//!     assert_eq!(args.arg_DIR, "dest/".to_string());
//! }
//! ```
//!
//! Wait. What did that just do? Yup. It *created a struct definition* for
//! you. In this case, the struct definition would look like this:
//!
//! ```ignore
//! #[deriving(Decodable, Show)]
//! struct Args {
//!     pub flag_archive: bool,
//!     pub arg_DEST: String,
//!     pub arg_DIR: String,
//!     pub arg_SOURCE: Vec<String>,
//! }
//! ```
//!
//! There are also some accompanying static methods defined, such as `parse`,
//! `parse_conf` and `parse_args`. These methods will parse command line
//! arguments, try to match them against your Docopt usage string and then
//! try to decode matched values into the struct.
//!
//! If you're thinking that this is too much magic, then you may be right. This
//! API should be considered experimental. I do not yet know what the right
//! level of magic is. Therefore, you can use an API that is less convenient
//! but with absolutely no magic. Here's the same `cp` example above, but
//! using a hash table instead:
//!
//! ```rust
//! extern crate docopt;
//!
//! fn main() {
//!     let config = docopt::DEFAULT_CONFIG.clone();
//!     let argv = &["-a", "file1", "file2", "dest/"];
//!     let args = docopt::docopt_args(config, argv, "
//! Usage: cp [-a] SOURCE DEST
//!        cp [-a] SOURCE... DIR
//! 
//! Options:
//!     -a, --archive  Copy everything.
//!     ");
//!     assert!(args.get_bool("-a") && args.get_bool("--archive"));
//!     assert_eq!(args.get_vec("SOURCE"), vec!["file1", "file2"]);
//!     assert_eq!(args.get_str("DIR"), "dest/");
//! }
//! ```
//!
//! # Command line arguments for `rustc`
//!
//! Here's an example (with a macro) showing a subset of `rustc`'s command
//! line arguments.
//!
//! ```rust
//! # #![feature(phase)]
//! # extern crate serialize;
//! # #[phase(plugin, link)] extern crate docopt_macros;
//! # extern crate docopt;
//! # use docopt::FlagParser;
//! docopt!(Args, "
//! Usage: rustc [options] [--cfg SPEC... -L PATH...] INPUT
//!        rustc (--help | --version)
//! 
//! Options:
//!     -h, --help         Show this message.
//!     --version          Show the version of rustc.
//!     --cfg SPEC         Configure the compilation environment.
//!     -L PATH            Add a directory to the library search path.
//!     --emit TYPE        Configure the output that rustc will produce.
//!                        Valid values: asm, ir, bc, obj, link.
//!     --opt-level LEVEL  Optimize with possible levels 0-3.
//! ")
//!
//! fn main() {
//!     let argv = &["--cfg", "a", "docopt.rs", "-L", ".", "-L.."];
//!     let args: Args = FlagParser::parse_args(docopt::DEFAULT_CONFIG.clone(), argv);
//!     assert_eq!(args.arg_INPUT, "docopt.rs".to_string());
//!     assert_eq!(args.flag_L, vec![".".to_string(), "..".to_string()]);
//!     assert_eq!(args.flag_cfg, vec!["a".to_string()]);
//! }
//! ```
//!
//! This example shows some of the more advanced features of Docopt. However,
//! there's still one large omission: data validation. In the above example,
//! *any* value can be given to `--emit` or `--opt-level`, but clearly, the
//! application defines a limited subset of all possible values.
//!
//! This is where data validation can help. In Docopt proper, it is a
//! *non-goal* to validate and convert the data. However, this implementation
//! provides some facilities to do just that. All you need to do is provide 
//! Rust types for the flags/arguments, and the decoder will do the rest. 
//! Here's an example of limiting the range of values for `--emit` and 
//! `--opt-level`:
//!
//! (Note the extra type annotations in the `docopt!` macro call.)
//!
//! ```rust
//! # #![feature(phase)]
//! # extern crate serialize;
//! # #[phase(plugin)] extern crate docopt_macros;
//! # extern crate docopt;
//! # use docopt::FlagParser;
//! docopt!(Args, "
//! Usage: rustc [options] [--cfg SPEC... -L PATH...] INPUT
//!        rustc (--help | --version)
//! 
//! Options:
//!     -h, --help         Show this message.
//!     --version          Show the version of rustc.
//!     --cfg SPEC         Configure the compilation environment.
//!     -L PATH            Add a directory to the library search path.
//!     --emit TYPE        Configure the output that rustc will produce.
//!                        Valid values: asm, ir, bc, obj, link.
//!     --opt-level LEVEL  Optimize with possible levels 0-3.
//! ", flag_opt_level: Option<OptLevel>, flag_emit: Option<Emit>)
//! 
//! // This is easy. The decoder will automatically restrict values to
//! // strings that match one of the enum variants.
//! #[deriving(Decodable, PartialEq, Show)]
//! enum Emit { Asm, Ir, Bc, Obj, Link }
//! 
//! // This one is harder because we want the user to specify an integer,
//! // but restrict it to a specific range. So we implement `Decodable`
//! // ourselves.
//! #[deriving(PartialEq, Show)]
//! enum OptLevel { Zero, One, Two, Three }
//! 
//! impl<E, D: serialize::Decoder<E>> serialize::Decodable<D, E> for OptLevel {
//!     fn decode(d: &mut D) -> Result<OptLevel, E> {
//!         Ok(match try!(d.read_uint()) {
//!             0 => Zero, 1 => One, 2 => Two, 3 => Three,
//!             // This partly defeats the purpose of this example, but
//!             // hopefully showing a wart will encourage people to help
//!             // me fix it.
//!             _ => fail!("How to CONVENIENTLY create value with type `E`?"),
//!         })
//!     }
//! }
//!
//! fn main() {
//!     let argv = &["--opt-level", "2", "--emit=ir", "docopt.rs"];
//!     let args: Args = FlagParser::parse_args(docopt::DEFAULT_CONFIG.clone(), argv);
//!     assert_eq!(args.flag_opt_level, Some(Two));
//!     assert_eq!(args.flag_emit, Some(Ir));
//! }
//! ```
//!
//! If you tried to pass, for example, `--emit=whatever`, then the decoding
//! would fail.

#![crate_name = "docopt"]
#![crate_type = "rlib"] 
#![crate_type = "dylib"] 
#![experimental]
#![license = "UNLICENSE"]
#![doc(html_root_url = "http://burntsushi.net/rustdoc/docopt")]

#![allow(dead_code, unused_variable, unused_imports)]
#![allow(visible_private_types)]
#![feature(plugin_registrar, macro_rules, phase, quote)]

extern crate debug;
extern crate libc;
// #[cfg(test)] 
#[phase(plugin, link)]
extern crate log;
extern crate regex;
#[phase(plugin)] extern crate regex_macros;
extern crate serialize;

use std::collections::HashMap;
use std::fmt;
use std::from_str::{FromStr, from_str};
use std::num;
use serialize::Decodable;
use parse::Parser;
use synonym::SynonymMap;

macro_rules! werr(
    ($($arg:tt)*) => (
        match std::io::stderr().write_str(format!($($arg)*).as_slice()) {
            Ok(_) => (),
            Err(err) => fail!("{}", err),
        }
    )
)

/// Represents the different types of Docopt errors.
///
/// These types correspond to the following operations: parsing a Docopt
/// usage string, parsing an argv arguments, not matching an argv against
/// a Docopt pattern and decoding a successful match into a struct.
pub enum Error {
    Usage(String),
    Argv(String),
    NoMatch,
    Decode(String),
}

impl fmt::Show for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &NoMatch => write!(f, "Invalid arguments."),
            &Usage(ref s) | &Argv(ref s) | &Decode(ref s) => write!(f, "{}", s),
        }
    }
}

/// A set of Docopt usage patterns.
///
/// This can be used to match command line arguments to produce a `ValueMap`.
#[deriving(Show)]
pub struct Docopt {
    p: Parser,
    conf: Config,
}

/// Configure how command line arguments are parsed.
#[deriving(Clone, Show)]
pub struct Config {
    /// When true, flags must appear before positional arguments. That is,
    /// after the first positional argument is seen, no more flags are allowed.
    pub options_first: bool,
    /// When true, if `--help` is present, then the full Docopt usage string
    /// initially given is shown.
    /// Note that for this to work, `--help` has to be a valid usage pattern.
    pub help: bool,
    /// If set, shows the given string when `--version` is present.
    /// Note that for this to work, `--version` has to be a valid usage pattern.
    pub version: Option<String>,
}

/// The default configuration used in covenience functions such as `docopt`.
pub static DEFAULT_CONFIG: Config = Config {
    options_first: false,
    help: true,
    version: None,
};

impl Docopt {
    /// Parse the Docopt usage string given with the configuration specified.
    /// If there was an error processing the usage string, then a `Usage`
    /// error is returned.
    pub fn new(conf: Config, doc: &str) -> Result<Docopt, Error> {
        Parser::new(doc)
            .map(|p| Docopt { p: p, conf: conf.clone() })
            .or_else(|s| Err(Usage(s)))
    }

    /// Parse command line arguments and try to match them against a usage
    /// pattern specified in the Docopt string. If parsing the command line
    /// arguments fails, then an `Argv` error is returned. If parsing succeeds
    /// but there is no match, then a `NoMatch` error is returned.
    ///
    /// The `args` given *must* start with the first argument and *not* with
    /// the program's name. e.g., `["cp", "src", "dest"]` is wrong while
    /// `["src", "dest"]` is correct.
    pub fn argv(&self, args: &[&str]) -> Result<ValueMap, Error> {
        self.p.parse_argv(args, self.conf.options_first)
            .or_else(|s| Err(Argv(s)))
            .and_then(|argv|
                self.p.matches(&argv)
                    .map(|m| Ok(ValueMap { map: m }))
                    .unwrap_or_else(|| Err(NoMatch)))
    }

    #[inline]
    pub fn parser<'a>(&'a self) -> &'a Parser {
        &self.p
    }
}

/// A map containing matched values from command line arguments.
///
/// The keys are just as specified in Docopt: `--flag` for a long flag or
/// `-f` for a short flag. (If `-f` is a synonym for `--flag`, then either
/// key will work.) `ARG` or `<arg>` specify a positional argument and `cmd`
/// specifies a command.
#[deriving(Clone)]
pub struct ValueMap {
    map: SynonymMap<String, Value>,
}

/// A matched command line value.
///
/// The value can be a boolean, counted repetition, a plain string or a list
/// of strings.
///
/// The various `as_{bool,count,str,vec}` methods provide convenient access
/// to values without destructuring manually.
#[deriving(Clone, PartialEq, Show)]
pub enum Value {
    Switch(bool),
    Counted(uint),
    Plain(Option<String>),
    List(Vec<String>),
}

impl Value {
    /// Returns the value as a bool.
    ///
    /// Counted repetitions are `false` if `0` and `true` otherwise.
    /// Plain strings are `true` if present and `false` otherwise.
    /// Lists are `true` if non-empty and `false` otherwise.
    pub fn as_bool(&self) -> bool {
        match self {
            &Switch(b) => b,
            &Counted(n) => n > 0,
            &Plain(None) => false,
            &Plain(Some(_)) => true,
            &List(ref vs) => !vs.is_empty(),
        }
    }

    /// Returns the value as a count of the number of times it occurred.
    ///
    /// Booleans are `1` if `true` and `0` otherwise.
    /// Plain strings are `1` if present and `0` otherwise.
    /// Lists correspond to its length.
    pub fn as_count(&self) -> uint {
        match self {
            &Switch(b) => if b { 1 } else { 0 },
            &Counted(n) => n,
            &Plain(None) => 0,
            &Plain(Some(_)) => 1,
            &List(ref vs) => vs.len(),
        }
    }

    /// Returns the value as a string.
    ///
    /// All values return an empty string except for a non-empty plain string.
    pub fn as_str<'a>(&'a self) -> &'a str {
        match self {
            &Switch(_) | &Counted(_) | &Plain(None) | &List(_) => "",
            &Plain(Some(ref s)) => s.as_slice(),
        }
    }

    /// Returns the value as a list of strings.
    ///
    /// Booleans, repetitions and empty strings correspond to an empty list.
    /// Plain strings correspond to a list of length `1`.
    pub fn as_vec<'a>(&'a self) -> Vec<&'a str> {
        match self {
            &Switch(_) | &Counted(_) | &Plain(None) => vec!(),
            &Plain(Some(ref s)) => vec!(s.as_slice()),
            &List(ref vs) => vs.iter().map(|s| s.as_slice()).collect(),
        }
    }
}

impl ValueMap {
    /// Tries to decode the map of values into a struct.
    ///
    /// This method should always be called to decode a `ValueMap` into
    /// a struct. All fields of the struct must map to a corresponding key
    /// in the `ValueMap`. To this end, each member must have a special prefix
    /// corresponding to the different kinds of patterns in Docopt. There are
    /// three prefixes: `flag_`, `arg_` and `cmd_` which respectively 
    /// correspond to short/long flags, positional arguments and commands.
    ///
    /// This documentation needs to be fleshed out more.
    ///
    /// # Example
    ///
    /// ```rust
    /// # extern crate serialize;
    /// # extern crate docopt;
    /// # fn main() {
    /// use docopt::{DEFAULT_CONFIG, docopt_args};
    ///
    /// #[deriving(Decodable)]
    /// struct Args {
    ///   cmd_build: bool,
    ///   cmd_test: bool,
    ///   flag_verbose: bool,
    ///   flag_h: bool,
    /// }
    /// 
    /// let argv = &["-v", "build"];
    /// let doc = docopt_args(DEFAULT_CONFIG.clone(), argv, "
    /// Usage: cargo [options] (build | test)
    ///        cargo --help
    ///
    /// Options: -v, --verbose
    ///          -h, --help
    /// ");
    /// let args: Args = doc.decode().unwrap();
    /// assert!(args.cmd_build && !args.cmd_test
    ///         && args.flag_verbose && !args.flag_h);
    /// # }
    /// ```
    ///
    /// Note that in the above example, `flag_h` is used but `flag_help`
    /// could also be used. (In fact, both could be used at the same time.)
    ///
    /// In this example, only the `bool` type was used, but any type satisfying
    /// the `Decodable` trait is valid.
    pub fn decode<'a, T: Decodable<Decoder<'a>, Error>>
                 (&'a self) -> Result<T, Error> {
        Decodable::decode(&mut Decoder { vals: self, stack: vec!() })
    }

    /// The same as `decode`, except if there is an error, it is logged to
    /// `stderr` and the current program will *unsafely* exit with an error
    /// code of `1`.
    pub fn decode_must<'a, T: Decodable<Decoder<'a>, Error>>(&'a self) -> T {
        match self.decode() {
            Ok(v) => v,
            Err(err) => {
                werr!("{}\n", err);
                exit(1);
            }
        }
    }

    /// Finds the value corresponding to `key` and calls `as_bool()` on it.
    /// If the key does not exist, `false` is returned.
    pub fn get_bool(&self, key: &str) -> bool {
        self.find(&key).map(|v| v.as_bool()).unwrap_or(false)
    }
    /// Finds the value corresponding to `key` and calls `as_count()` on it.
    /// If the key does not exist, `0` is returned.
    pub fn get_count(&self, key: &str) -> uint {
        self.find(&key).map(|v| v.as_count()).unwrap_or(0)
    }
    /// Finds the value corresponding to `key` and calls `as_str()` on it.
    /// If the key does not exist, `""` is returned.
    pub fn get_str<'a>(&'a self, key: &str) -> &'a str {
        self.find(&key).map(|v| v.as_str()).unwrap_or("")
    }
    /// Finds the value corresponding to `key` and calls `as_vec()` on it.
    /// If the key does not exist, `vec!()` is returned.
    pub fn get_vec<'a>(&'a self, key: &str) -> Vec<&'a str> {
        self.find(&key).map(|v| v.as_vec()).unwrap_or(vec!())
    }

    /// Converts a Docopt key to a struct field name.
    /// This makes a half-hearted attempt at making the key a valid struct
    /// field name (like replacing `-` with `_`), but it does not otherwise
    /// guarantee that the result is a valid struct field name.
    pub fn key_to_struct_field(name: &str) -> String {
        fn sanitize(name: &str) -> String {
            name.replace("-", "_")
        }

        let r = regex!(r"^(?:--?(?P<flag>\S+)|(?:(?P<argu>\p{Lu}+)|<(?P<argb>[^>]+)>)|(?P<cmd>\S+))$");
        r.replace(name, |cap: &regex::Captures| {
            let (flag, cmd) = (cap.name("flag"), cap.name("cmd"));
            let (argu, argb) = (cap.name("argu"), cap.name("argb"));
            let (prefix, name) =
                if !flag.is_empty() {
                    ("flag_", flag)
                } else if !argu.is_empty() {
                    ("arg_", argu)
                } else if !argb.is_empty() {
                    ("arg_", argb)
                } else if !cmd.is_empty() {
                    ("cmd_", cmd)
                } else {
                    fail!("Unknown ValueMap key: '{}'", name)
                };
            prefix.to_string().append(sanitize(name).as_slice())
        })
    }

    /// Converts a struct field name to a Docopt key.
    pub fn struct_field_to_key(field: &str) -> String {
        fn desanitize(name: &str) -> String {
            name.replace("_", "-")
        }
        let name =
            if field.starts_with("flag_") {
                let name = regex!(r"^flag_").replace(field, "");
                if name.len() == 1 {
                    "-".to_string().append(name.as_slice())
                } else {
                    "--".to_string().append(name.as_slice())
                }
            } else if field.starts_with("arg_") {
                let name = regex!(r"^arg_").replace(field, "");
                if regex!(r"^\p{Lu}+$").is_match(name.as_slice()) {
                    name
                } else {
                    "<".to_string().append(name.as_slice()).append(">")
                }
            } else if field.starts_with("cmd_") {
                { regex!(r"^cmd_") }.replace(field, "")
            } else {
                fail!("Unrecognized struct field: '{}'", field)
            };
        desanitize(name.as_slice())
    }
}

impl Collection for ValueMap {
    fn len(&self) -> uint { self.map.len() }
}

impl<'k> Map<&'k str, Value> for ValueMap {
    fn find<'a>(&'a self, key: & &'k str) -> Option<&'a Value> {
        self.map.find(&key.to_string())
    }
}

impl fmt::Show for ValueMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_empty() {
            return write!(f, "{{EMPTY}}");
        }

        // This is a little crazy, but we want to group synonyms with
        // their keys and sort them for predictable output.
        let reverse: HashMap<&String, &String> =
            self.map.synonyms().map(|(from, to)| (to, from)).collect();
        let mut keys: Vec<&String> = self.map.keys().collect();
        keys.sort();
        let mut first = true;
        for &k in keys.iter() {
            if !first { try!(write!(f, "\n")); } else { first = false; }
            match reverse.find(&k) {
                None => {
                    try!(write!(f, "{} => {}", k, self.map.get(k)))
                }
                Some(s) => {
                    try!(write!(f, "{}, {} => {}", s, k, self.map.get(k)))
                }
            }
        }
        Ok(())
    }
}

struct Decoder<'a> {
    vals: &'a ValueMap,
    stack: Vec<DecoderItem>,
}

#[deriving(Show)]
struct DecoderItem {
    key: String,
    struct_field: String,
    val: Option<Value>,
}

macro_rules! derr(
    ($($arg:tt)*) => (return Err(Decode(format!($($arg)*))))
)

impl<'a> Decoder<'a> {
    fn push(&mut self, struct_field: &str) {
        let key = ValueMap::struct_field_to_key(struct_field);
        self.stack.push(DecoderItem {
            key: key.clone(),
            struct_field: struct_field.to_string(),
            val: self.vals.find(&key.as_slice()).map(|v| v.clone()),
        });
    }

    fn pop(&mut self) -> Result<DecoderItem, Error> {
        match self.stack.pop() {
            None => derr!("Could not decode value into unknown key."),
            Some(it) => Ok(it),
        }
    }

    fn pop_key_val(&mut self) -> Result<(String, Value), Error> {
        let it = try!(self.pop());
        match it.val {
            None => derr!("Could not find argument '{}' (from struct \
                           field '{}').", it.key, it.struct_field),
            Some(v) => Ok((it.key, v)),
        }
    }

    fn pop_val(&mut self) -> Result<Value, Error> {
        let (_, v) = try!(self.pop_key_val());
        Ok(v)
    }

    fn to_number<T: FromStr + NumCast>
                (&mut self, expect: &str) -> Result<T, Error> {
        let (k, v) = try!(self.pop_key_val());
        match v {
            Counted(n) => Ok(num::cast(n).unwrap()),
            _ => {
                match from_str(v.as_str()) {
                    None => derr!("Could not decode '{}' to {} for '{}'.",
                                  v.as_str(), expect, k),
                    Some(v) => Ok(v),
                }
            }
        }
    }
}

impl<'a> serialize::Decoder<Error> for Decoder<'a> {
    fn read_nil(&mut self) -> Result<(), Error> {
        // I don't know what the right thing is here, so just fail for now.
        fail!("I don't know how to read into a nil value.")
    }
    fn read_uint(&mut self) -> Result<uint, Error> {
        self.to_number("uint")
    }
    fn read_u64(&mut self) -> Result<u64, Error> {
        self.to_number("u64")
    }
    fn read_u32(&mut self) -> Result<u32, Error> {
        self.to_number("u32")
    }
    fn read_u16(&mut self) -> Result<u16, Error> {
        self.to_number("u16")
    }
    fn read_u8(&mut self) -> Result<u8, Error> {
        self.to_number("u8")
    }
    fn read_int(&mut self) -> Result<int, Error> {
        self.to_number("int")
    }
    fn read_i64(&mut self) -> Result<i64, Error> {
        self.to_number("i64")
    }
    fn read_i32(&mut self) -> Result<i32, Error> {
        self.to_number("i32")
    }
    fn read_i16(&mut self) -> Result<i16, Error> {
        self.to_number("i16")
    }
    fn read_i8(&mut self) -> Result<i8, Error> {
        self.to_number("i8")
    }
    fn read_bool(&mut self) -> Result<bool, Error> {
        self.pop_val().map(|v| v.as_bool())
    }
    fn read_f64(&mut self) -> Result<f64, Error> {
        self.to_number("f64")
    }
    fn read_f32(&mut self) -> Result<f32, Error> {
        self.to_number("f32")
    }
    fn read_char(&mut self) -> Result<char, Error> {
        let (k, v) = try!(self.pop_key_val());
        let vstr = v.as_str();
        match vstr.len() {
            1 => Ok(vstr.char_at(0)),
            _ => derr!("Could not decode '{}' into char for '{}'.", vstr, k),
        }
    }
    fn read_str(&mut self) -> Result<String, Error> {
        self.pop_val().map(|v| v.as_str().to_string())
    }
    fn read_enum<T>(&mut self, name: &str,
                    f: |&mut Decoder<'a>| -> Result<T, Error>)
                    -> Result<T, Error> {
        f(self)
    }
    fn read_enum_variant<T>(&mut self, names: &[&str],
                            f: |&mut Decoder<'a>, uint| -> Result<T, Error>)
                            -> Result<T, Error> {
        let v = try!(self.pop_val());
        let vstr = to_lower(v.as_str());
        let i =
            match names.iter().map(|&n| to_lower(n)).position(|n| n == vstr) {
                Some(i) => i,
                None => {
                    derr!("Could not match '{}' with any of \
                           the allowed variants: {}", vstr, names)
                }
            };
        f(self, i)
    }
    fn read_enum_variant_arg<T>(
        &mut self, a_idx: uint,
        f: |&mut Decoder<'a>| -> Result<T, Error>) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_enum_struct_variant<T>(
        &mut self, names: &[&str],
        f: |&mut Decoder<'a>, uint| -> Result<T, Error>) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_enum_struct_variant_field<T>(
        &mut self, f_name: &str, f_idx: uint,
        f: |&mut Decoder<'a>| -> Result<T, Error>) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_struct<T>(&mut self, s_name: &str, len: uint,
                      f: |&mut Decoder<'a>| -> Result<T, Error>)
                      -> Result<T, Error> {
        f(self)
    }
    fn read_struct_field<T>(&mut self, f_name: &str, f_idx: uint,
                            f: |&mut Decoder<'a>| -> Result<T, Error>)
                            -> Result<T, Error> {
        self.push(f_name);
        f(self)
    }
    fn read_tuple<T>(&mut self,
                     f: |&mut Decoder<'a>, uint| -> Result<T, Error>)
                     -> Result<T, Error> {
        unimplemented!()
    }
    fn read_tuple_arg<T>(&mut self, a_idx: uint,
                         f: |&mut Decoder<'a>| -> Result<T, Error>)
                         -> Result<T, Error> {
        unimplemented!()
    }
    fn read_tuple_struct<T>(&mut self, s_name: &str,
                            f: |&mut Decoder<'a>, uint| -> Result<T, Error>)
                            -> Result<T, Error> {
        unimplemented!()
    }
    fn read_tuple_struct_arg<T>(&mut self, a_idx: uint,
                                f: |&mut Decoder<'a>| -> Result<T, Error>)
                                -> Result<T, Error> {
        unimplemented!()
    }
    fn read_option<T>(&mut self,
                      f: |&mut Decoder<'a>, bool| -> Result<T, Error>)
                      -> Result<T, Error> {
        let option =
            match self.stack.last() {
                None => derr!("Could not decode value into unknown key."),
                Some(it) => it.val.as_ref()
                                  .map(|v| v.as_bool())
                                  .unwrap_or(false),
            };
        f(self, option)
    }
    fn read_seq<T>(&mut self,
                   f: |&mut Decoder<'a>, uint| -> Result<T, Error>)
                   -> Result<T, Error> {
        let it = try!(self.pop());
        let list = it.val.unwrap_or(List(vec!()));
        let vals = list.as_vec();
        for val in vals.iter().rev() {
            self.stack.push(DecoderItem {
                key: it.key.clone(),
                struct_field: it.struct_field.clone(),
                val: Some(Plain(Some(val.to_string()))),
            })
        }
        f(self, vals.len())
    }
    fn read_seq_elt<T>(&mut self, idx: uint,
                       f: |&mut Decoder<'a>| -> Result<T, Error>)
                       -> Result<T, Error> {
        f(self)
    }
    fn read_map<T>(&mut self,
                   f: |&mut Decoder<'a>, uint| -> Result<T, Error>)
                   -> Result<T, Error> {
        unimplemented!()
    }
    fn read_map_elt_key<T>(&mut self, idx: uint,
                           f: |&mut Decoder<'a>| -> Result<T, Error>)
                           -> Result<T, Error> {
        unimplemented!()
    }
    fn read_map_elt_val<T>(&mut self, idx: uint,
                           f: |&mut Decoder<'a>| -> Result<T, Error>)
                           -> Result<T, Error> {
        unimplemented!()
    }
}

pub trait FlagParser {
    fn parse_args(conf: Config, args: &[&str]) -> Self;
    fn parse() -> Self { FlagParser::parse_conf(DEFAULT_CONFIG.clone()) }
    fn parse_conf(conf: Config) -> Self {
        with_os_argv(|argv| FlagParser::parse_args(conf.clone(), argv))
    }
}

/// Matches the current `argv` against the Docopt string given using the
/// default configuration.
///
/// If an error occurs, an appropriate message is written to `stderr` and
/// the program will exit unsafely.
pub fn docopt(doc: &str) -> ValueMap {
    docopt_conf(DEFAULT_CONFIG.clone(), doc)
}

/// Matches the current `argv` against the Docopt string given with the given
/// configuration.
///
/// If an error occurs, an appropriate message is written to `stderr` and
/// the program will exit unsafely.
pub fn docopt_conf(conf: Config, doc: &str) -> ValueMap {
    with_os_argv(|argv| docopt_args(conf.clone(), argv, doc))
}

/// Matches the given `args` against the Docopt string given with the given
/// configuration.
///
/// Note that `args` should *not* begin with the program name. e.g.,
/// `["cp", "src", "dst"]` is wrong while `["src", "dst"]` is correct.
///
/// If an error occurs, an appropriate message is written to `stderr` and
/// the program will exit unsafely.
pub fn docopt_args(conf: Config, args: &[&str], doc: &str) -> ValueMap {
    match Docopt::new(conf, doc) {
        Ok(dopt) => convenient_parse_args(&dopt, args),
        Err(err) => fail!("{}", err),
    }
}

fn with_os_argv<T>(f: |&[&str]| -> T) -> T {
    let os_argv = std::os::args();
    let argv: Vec<&str> = os_argv.iter().skip(1).map(|s|s.as_slice()).collect();
    f(argv.as_slice())
}

fn convenient_parse_args(dopt: &Docopt, argv: &[&str]) -> ValueMap {
    let vals = dopt.argv(argv).unwrap_or_else(|err| {
        werr!("{}\n", err);
        werr!("{}\n", dopt.p.usage.as_slice().trim());
        exit(1);
    });
    if dopt.conf.help && vals.get_bool("--help") {
        werr!("{}\n", dopt.p.full_doc.as_slice().trim());
        exit(1);
    }
    match dopt.conf.version {
        Some(ref v) if vals.get_bool("--version") => {
            werr!("{}\n", v);
            exit(0);
        }
        _ => {},
    }
    vals
}

fn argv_has_help(argv: &[&str]) -> bool {
    argv.contains(&"-h") || argv.contains(&"--help")
}

fn argv_has_version(argv: &[&str]) -> bool {
    argv.contains(&"--version")
}

fn to_lower(s: &str) -> String {
    s.chars().map(|c| c.to_lowercase()).collect()
}

// I've been warned that this is wildly unsafe.
// Unless there's a reasonable alternative, I'm inclined to say that this is
// the price you pay for convenience.
fn exit(code: uint) -> ! {
    unsafe { libc::exit(code as libc::c_int) }
}

pub mod parse;
mod synonym;
#[cfg(test)]
mod test;

