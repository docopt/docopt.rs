//! Docopt for Rust. This implementation conforms to the
//! [official description of Docopt](http://docopt.org/) and
//! [passes its test suite](https://github.com/docopt/docopt/pull/201).
//!
//! This library is [on GitHub](https://github.com/docopt/docopt.rs).
//!
//! Fundamentally, Docopt is a command line argument parser. The detail that
//! distinguishes it from most parsers is that the parser is derived from the
//! usage string. Here's a simple example:
//!
//! ```rust
//! #![feature(old_orphan_check)]
//! use docopt::Docopt;
//!
//! // Write the Docopt usage string.
//! static USAGE: &'static str = "
//! Usage: cp [-a] <source> <dest>
//!        cp [-a] <source>... <dir>
//!
//! Options:
//!     -a, --archive  Copy everything.
//! ";
//!
//! // The argv. Normally you'd just use `parse` which will automatically
//! // use `std::os::args()`.
//! let argv = || vec!["cp", "-a", "file1", "file2", "dest/"];
//!
//! // Parse argv and exit the program with an error message if it fails.
//! let args = Docopt::new(USAGE)
//!                   .and_then(|d| d.argv(argv().into_iter()).parse())
//!                   .unwrap_or_else(|e| e.exit());
//!
//! // Now access your argv values. Synonyms work just fine!
//! assert!(args.get_bool("-a") && args.get_bool("--archive"));
//! assert_eq!(args.get_vec("<source>"), vec!["file1", "file2"]);
//! assert_eq!(args.get_str("<dir>"), "dest/");
//! assert_eq!(args.get_str("<dest>"), "");
//! ```
//!
//! # Type based decoding
//!
//! Often, command line values aren't just strings or booleans---sometimes
//! they are integers, or enums, or something more elaborate. Using the
//! standard Docopt interface can be inconvenient for this purpose, because
//! you'll need to convert all of the values explicitly. Instead, this crate
//! provides a `Decoder` that converts an `ArgvMap` to your custom struct.
//! Here is the same example as above using type based decoding:
//!
//! ```rust
//! #![feature(old_orphan_check)]
//! # extern crate docopt;
//! # extern crate "rustc-serialize" as rustc_serialize;
//! # fn main() {
//! use docopt::Docopt;
//!
//! // Write the Docopt usage string.
//! static USAGE: &'static str = "
//! Usage: cp [-a] <source> <dest>
//!        cp [-a] <source>... <dir>
//!
//! Options:
//!     -a, --archive  Copy everything.
//! ";
//!
//! #[derive(RustcDecodable)]
//! struct Args {
//!     arg_source: Vec<String>,
//!     arg_dest: String,
//!     arg_dir: String,
//!     flag_archive: bool,
//! }
//!
//! let argv = || vec!["cp", "-a", "file1", "file2", "dest/"];
//! let args: Args = Docopt::new(USAGE)
//!                         .and_then(|d| d.argv(argv().into_iter()).decode())
//!                         .unwrap_or_else(|e| e.exit());
//!
//! // Now access your argv values.
//! fn s(x: &str) -> String { x.to_string() }
//! assert!(args.flag_archive);
//! assert_eq!(args.arg_source, vec![s("file1"), s("file2")]);
//! assert_eq!(args.arg_dir, s("dest/"));
//! assert_eq!(args.arg_dest, s(""));
//! # }
//! ```
//!
//! # Command line arguments for `rustc`
//!
//! Here's an example with a subset of `rustc`'s command line arguments that
//! shows more of Docopt and some of the benefits of type based decoding.
//!
//! ```rust
//! #![feature(old_orphan_check)]
//! # extern crate docopt;
//! # extern crate "rustc-serialize" as rustc_serialize;
//! # fn main() {
//! # #![allow(non_snake_case)]
//! use docopt::Docopt;
//!
//! // Write the Docopt usage string.
//! static USAGE: &'static str = "
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
//! ";
//!
//! #[derive(RustcDecodable)]
//! struct Args {
//!     arg_INPUT: String,
//!     flag_emit: Option<Emit>,
//!     flag_opt_level: Option<OptLevel>,
//!     flag_cfg: Vec<String>,
//!     flag_L: Vec<String>,
//!     flag_help: bool,
//!     flag_version: bool,
//! }
//!
//! // This is easy. The decoder will automatically restrict values to
//! // strings that match one of the enum variants.
//! #[derive(RustcDecodable)]
//! # #[derive(PartialEq, Show)]
//! enum Emit { Asm, Ir, Bc, Obj, Link }
//!
//! // This one is harder because we want the user to specify an integer,
//! // but restrict it to a specific range. So we implement `Decodable`
//! // ourselves.
//! # #[derive(PartialEq, Show)]
//! enum OptLevel { Zero, One, Two, Three }
//!
//! impl<E, D> rustc_serialize::Decodable<D, E> for OptLevel
//!         where D: rustc_serialize::Decoder<E> {
//!     fn decode(d: &mut D) -> Result<OptLevel, E> {
//!         Ok(match try!(d.read_uint()) {
//!             0 => OptLevel::Zero, 1 => OptLevel::One,
//!             2 => OptLevel::Two, 3 => OptLevel::Three,
//!             n => {
//!                 let err = format!(
//!                     "Could not decode '{}' as opt-level.", n);
//!                 return Err(d.error(err.as_slice()));
//!             }
//!         })
//!     }
//! }
//!
//! let argv = || vec!["rustc", "-L", ".", "-L", "..", "--cfg", "a",
//!                             "--opt-level", "2", "--emit=ir", "docopt.rs"];
//! let args: Args = Docopt::new(USAGE)
//!                         .and_then(|d| d.argv(argv().into_iter()).decode())
//!                         .unwrap_or_else(|e| e.exit());
//!
//! // Now access your argv values.
//! fn s(x: &str) -> String { x.to_string() }
//! assert_eq!(args.arg_INPUT, "docopt.rs".to_string());
//! assert_eq!(args.flag_L, vec![s("."), s("..")]);
//! assert_eq!(args.flag_cfg, vec![s("a")]);
//! assert_eq!(args.flag_opt_level, Some(OptLevel::Two));
//! assert_eq!(args.flag_emit, Some(Emit::Ir));
//! # }
//! ```
//!
//! # The `docopt!` macro
//!
//! This package comes bundled with an additional crate, `docopt_macros`,
//! which provides a `docopt!` syntax extension. Its purpose is to automate
//! the creation of a Rust struct from a Docopt usage string. In particular,
//! this provides a single point of truth about the definition of command line
//! arguments in your program.
//!
//! Another advantage of using the macro is that errors in your Docopt usage
//! string will be caught at compile time. Stated differently, your program
//! will not compile with an invalid Docopt usage string.
//!
//! The example above using type based decoding can be simplified to this:
//!
//! ```ignore
//! #![feature(phase)]
//!
//! extern crate "rustc-serialize" as rustc_serialize;
//!
//! extern crate docopt;
//! #[phase(plugin)] extern crate docopt_macros;
//!
//! // Write the Docopt usage string with the `docopt!` macro.
//! docopt!(Args, "
//! Usage: cp [-a] <source> <dest>
//!        cp [-a] <source>... <dir>
//!
//! Options:
//!     -a, --archive  Copy everything.
//! ")
//!
//! fn main() {
//!     let argv = || vec!["cp", "-a", "file1", "file2", "dest/"];
//!
//!     // Your `Args` struct has a single static method defined on it,
//!     // `docopt`, which will return a normal `Docopt` value.
//!     let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
//!
//!     // Now access your argv values.
//!     fn s(x: &str) -> String { x.to_string() }
//!     assert!(args.flag_archive);
//!     assert_eq!(args.arg_source, vec![s("file1"), s("file2")]);
//!     assert_eq!(args.arg_dir, s("dest/"));
//!     assert_eq!(args.arg_dest, s(""));
//! }
//! ```

#![crate_name = "docopt"]
#![doc(html_root_url = "http://burntsushi.net/rustdoc/docopt")]

#![experimental]
#![deny(missing_docs)]
#![feature(macro_rules)]
#![feature(old_orphan_check)]

extern crate libc;
extern crate regex;
extern crate "rustc-serialize" as rustc_serialize;

use std::borrow::ToOwned;
use std::collections::HashMap;
use std::error::Error as StdError;
use std::error::FromError;
use std::fmt;
use std::str::FromStr;
use std::num;
use std::num::NumCast;

use rustc_serialize::Decodable;

use parse::Parser;
use synonym::SynonymMap;

use Value::{Switch, Counted, Plain, List};
use Error::{Usage, Argv, NoMatch, Decode, WithProgramUsage, Help, Version};

macro_rules! werr(
    ($($arg:tt)*) => (
        match std::io::stderr().write_str(format!($($arg)*).as_slice()) {
            Ok(_) => (),
            Err(err) => panic!("{}", err),
        }
    )
);

// cheat until we get syntax extensions back :-(
macro_rules! regex(
    ($s:expr) => (regex::Regex::new($s).unwrap());
);

/// Represents the different types of Docopt errors.
///
/// This error type has a lot of variants. In the common case, you probably
/// don't care why Docopt has failed, and would rather just quit the program
/// and show an error message instead. The `exit` method defined on the `Error`
/// type will do just that. It will also set the exit code appropriately
/// (no error for `--help` or `--version`, but an error code for bad usage,
/// bad argv, no match or bad decode).
///
/// ### Example
///
/// Generally, you want to parse the usage string, try to match the argv
/// and then quit the program if there was an error reported at any point
/// in that process. This can be achieved like so:
///
/// ```no_run
/// use docopt::Docopt;
///
/// static USAGE: &'static str = "
/// Usage: ...
/// ";
///
/// let args = Docopt::new(USAGE)
///                   .and_then(|d| d.parse())
///                   .unwrap_or_else(|e| e.exit());
/// ```
pub enum Error {
    /// Parsing the usage string failed.
    ///
    /// This error can only be triggered by the programmer, i.e., the writer
    /// of the Docopt usage string. This error is usually indicative of a bug
    /// in your program.
    Usage(String),

    /// Parsing the argv specified failed.
    ///
    /// The payload is a string describing why the arguments provided could not
    /// be parsed.
    ///
    /// This is distinct from `NoMatch` because it will catch errors like
    /// using flags that aren't defined in the usage string.
    Argv(String),

    /// The given argv parsed successfully, but it did not match any example
    /// usage of the program.
    ///
    /// Regrettably, there is no descriptive message describing *why* the
    /// given argv didn't match any of the usage strings.
    NoMatch,

    /// This indicates a problem decoding a successful argv match into a
    /// decodable value.
    Decode(String),

    /// Parsing failed, and the program usage should be printed next to the
    /// failure message. Typically this wraps `Argv` and `NoMatch` errors.
    WithProgramUsage(Box<Error>, String),

    /// Decoding or parsing failed because the command line specified that the
    /// help message should be printed.
    Help,

    /// Decoding or parsing failed because the command line specified that the
    /// version should be printed
    ///
    /// The version is included as a payload to this variant.
    Version(String),
}

impl Error {
    /// Return whether this was a fatal error or not.
    ///
    /// Non-fatal errors include requests to print the help or version
    /// information of a program, while fatal errors include those such as
    /// failing to decode or parse.
    pub fn fatal(&self) -> bool {
        match *self {
            Help | Version(..) => false,
            Usage(..) | Argv(..) | NoMatch | Decode(..) => true,
            WithProgramUsage(ref b, _) => b.fatal(),
        }
    }

    /// Print this error and immediately exit the program.
    ///
    /// If the error is non-fatal (e.g., `Help` or `Version`), then the
    /// error is printed to stdout and the exit status will be `0`. Otherwise,
    /// when the error is fatal, the error is printed to stderr and the
    /// exit status will be `1`.
    pub fn exit(&self) -> ! {
        if self.fatal() {
            werr!("{}\n", self);
            exit(1)
        } else {
            println!("{}", self);
            exit(0)
        }
    }
}

impl fmt::Show for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            WithProgramUsage(ref other, ref usage) => {
                let other = other.to_string();
                if other.is_empty() {
                    write!(f, "{}", usage)
                } else {
                    write!(f, "{}\n\n{}", other, usage)
                }
            }
            Help => write!(f, ""),
            NoMatch => write!(f, "Invalid arguments."),
            Usage(ref s) | Argv(ref s) | Decode(ref s) | Version(ref s) => {
                write!(f, "{}", s)
            }
        }
    }
}

impl StdError for Error {
    fn description(&self) -> &str {
        match *self {
            Usage(..) => "invalid usage string",
            Argv(..) => "failed to parse specified argv",
            NoMatch => "could not match specified argv",
            Decode(..) => "failed to decode",
            WithProgramUsage(..) => "failed to parse specified argv",
            Help => "help message requested",
            Version(..) => "version message requested",
        }
    }

    fn detail(&self) -> Option<String> { Some(self.to_string()) }
    fn cause(&self) -> Option<&StdError> {
        match *self {
            WithProgramUsage(ref cause, _) => Some(&**cause as &StdError),
            _ => None,
        }
    }
}

impl FromError<Error> for Box<StdError> {
    fn from_error(err: Error) -> Box<StdError> {
        box err
    }
}

/// Encapsulate allocating of strings.
///
/// This is a temporary measure until the standard library provides more
/// impls for `std::string::IntoString`.
pub trait StrAllocating {
    /// Provide an owned String.
    fn into_str(self) -> String;
}

impl StrAllocating for String {
    fn into_str(self) -> String { self }
}

impl<'a> StrAllocating for &'a str {
    fn into_str(self) -> String { self.to_owned() }
}

/// The main Docopt type, which is constructed with a Docopt usage string.
///
/// This can be used to match command line arguments to produce a `ArgvMap`.
#[derive(Clone, Show)]
pub struct Docopt {
    p: Parser,
    argv: Option<Vec<String>>,
    options_first: bool,
    help: bool,
    version: Option<String>,
}

impl Docopt {
    /// Parse the Docopt usage string given.
    ///
    /// The `Docopt` value returned may be used immediately to parse command
    /// line arguments with a default configuration.
    ///
    /// If there was a problem parsing the usage string, a `Usage` error
    /// is returned.
    pub fn new<S: Str>(usage: S) -> Result<Docopt, Error> {
        Parser::new(usage.as_slice())
               .map_err(Usage)
               .map(|p| Docopt {
                   p: p,
                   argv: None,
                   options_first: false,
                   help: true,
                   version: None,
                })
    }

    /// Parse and decode the given argv.
    ///
    /// This is a convenience method for
    /// `parse().and_then(|vals| vals.decode())`.
    ///
    /// For details on how decoding works, please see the documentation for
    /// `ArgvMap`.
    pub fn decode<D>(&self) -> Result<D, Error>
                 where D: Decodable<Decoder, Error> {
        self.parse().and_then(|vals| vals.decode())
    }

    /// Parse command line arguments and try to match them against a usage
    /// pattern specified in the Docopt string.
    ///
    /// If there is a match, then an `ArgvMap` is returned, which maps
    /// flags, commands and arguments to values.
    ///
    /// If parsing the command line arguments fails, then an `Argv` error is
    /// returned. If parsing succeeds but there is no match, then a `NoMatch`
    /// error is returned. Both of these errors are always returned inside a
    /// `WithProgramUsage` error.
    ///
    /// If special handling of `help` or `version` is enabled (the former is
    /// enabled by default), then `Help` or `Version` errors are returned
    /// if `--help` or `--version` is present.
    pub fn parse(&self) -> Result<ArgvMap, Error> {
        let argv = self.argv.clone().unwrap_or_else(|| Docopt::get_argv());
        let vals = try!(
            self.p.parse_argv(argv, self.options_first)
                .map_err(|s| self.err_with_usage(Argv(s)))
                .and_then(|argv|
                    match self.p.matches(&argv) {
                        Some(m) => Ok(ArgvMap { map: m }),
                        None => Err(self.err_with_usage(NoMatch)),
                    }));
        if self.help && vals.get_bool("--help") {
            return Err(self.err_with_full_doc(Help));
        }
        match self.version {
            Some(ref v) if vals.get_bool("--version") => {
                return Err(Version(v.clone()))
            }
            _ => {},
        }
        Ok(vals)
    }

    /// Set the argv to be used for Docopt parsing.
    ///
    /// By default, when no argv is set, and it is automatically taken from
    /// `std::os::args()`.
    ///
    /// The `argv` given *must* be the full set of `argv` passed to the
    /// program. e.g., `["cp", "src", "dest"]` is right while `["src", "dest"]`
    /// is wrong.
    pub fn argv<I, S>(mut self, argv: I) -> Docopt
               where I: Iterator<Item=S>, S: StrAllocating {
        self.argv = Some(argv.skip(1).map(|s| s.into_str()).collect());
        self
    }

    /// Enables the "options first" Docopt behavior.
    ///
    /// The options first behavior means that all flags *must* appear before
    /// position arguments. That is, after the first position argument is
    /// seen, all proceeding arguments are interpreted as positional
    /// arguments unconditionally.
    pub fn options_first(mut self, yes: bool) -> Docopt {
        self.options_first = yes;
        self
    }

    /// Enables automatic handling of `--help`.
    ///
    /// When this is enabled and `--help` appears anywhere in the arguments,
    /// then a `Help` error will be returned. You may then use the `exit`
    /// method on the error value to conveniently quit the program (which will
    /// print the full usage string to stdout).
    ///
    /// Note that for this to work, `--help` must be a valid pattern.
    ///
    /// When disabled, there is no special handling of `--help`.
    pub fn help(mut self, yes: bool) -> Docopt {
        self.help = yes;
        self
    }

    /// Enables automatic handling of `--version`.
    ///
    /// When this is enabled and `--version` appears anywhere in the arguments,
    /// then a `Version(s)` error will be returned, where `s` is the string
    /// given here. You may then use the `exit` method on the error value to
    /// convenient quit the program (which will print the version to stdout).
    ///
    /// When disabled (a `None` value), there is no special handling of
    /// `--version`.
    pub fn version(mut self, version: Option<String>) -> Docopt {
        self.version = version;
        self
    }

    #[doc(hidden)]
    // Exposed for use in `docopt_macros`.
    pub fn parser<'a>(&'a self) -> &'a Parser {
        &self.p
    }

    fn err_with_usage(&self, e: Error) -> Error {
        WithProgramUsage(box e, self.p.usage.as_slice().trim().to_string())
    }

    fn err_with_full_doc(&self, e: Error) -> Error {
        WithProgramUsage(box e, self.p.full_doc.as_slice().trim().to_string())
    }

    fn get_argv() -> Vec<String> {
        ::std::os::args().as_slice().slice_from(1).to_vec()
    }
}

/// A map containing matched values from command line arguments.
///
/// The keys are just as specified in Docopt: `--flag` for a long flag or
/// `-f` for a short flag. (If `-f` is a synonym for `--flag`, then either
/// key will work.) `ARG` or `<arg>` specify a positional argument and `cmd`
/// specifies a command.
#[derive(Clone)]
pub struct ArgvMap {
    map: SynonymMap<String, Value>,
}

impl ArgvMap {
    /// Tries to decode the map of values into a struct.
    ///
    /// This method should always be called to decode a `ArgvMap` into
    /// a struct. All fields of the struct must map to a corresponding key
    /// in the `ArgvMap`. To this end, each member must have a special prefix
    /// corresponding to the different kinds of patterns in Docopt. There are
    /// three prefixes: `flag_`, `arg_` and `cmd_` which respectively
    /// correspond to short/long flags, positional arguments and commands.
    ///
    /// If a Docopt item has a `-` in its name, then it is converted to an `_`.
    ///
    /// # Example
    ///
    /// ```rust
    /// #![feature(old_orphan_check)]
    /// # extern crate docopt;
    /// # extern crate "rustc-serialize" as rustc_serialize;
    /// # fn main() {
    /// use docopt::Docopt;
    ///
    /// static USAGE: &'static str = "
    /// Usage: cargo [options] (build | test)
    ///        cargo --help
    ///
    /// Options: -v, --verbose
    ///          -h, --help
    /// ";
    ///
    /// #[derive(RustcDecodable)]
    /// struct Args {
    ///   cmd_build: bool,
    ///   cmd_test: bool,
    ///   flag_verbose: bool,
    ///   flag_h: bool,
    /// }
    ///
    /// let argv = || vec!["cargo", "build", "-v"].into_iter();
    /// let args: Args = Docopt::new(USAGE)
    ///                         .and_then(|d| d.argv(argv()).decode())
    ///                         .unwrap_or_else(|e| e.exit());
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
    pub fn decode<T: Decodable<Decoder, Error>>
                 (self) -> Result<T, Error> {
        Decodable::decode(&mut Decoder { vals: self, stack: vec!() })
    }

    /// Finds the value corresponding to `key` and calls `as_bool()` on it.
    /// If the key does not exist, `false` is returned.
    pub fn get_bool(&self, key: &str) -> bool {
        self.find(key).map(|v| v.as_bool()).unwrap_or(false)
    }

    /// Finds the value corresponding to `key` and calls `as_count()` on it.
    /// If the key does not exist, `0` is returned.
    pub fn get_count(&self, key: &str) -> uint {
        self.find(key).map(|v| v.as_count()).unwrap_or(0)
    }

    /// Finds the value corresponding to `key` and calls `as_str()` on it.
    /// If the key does not exist, `""` is returned.
    pub fn get_str<'a>(&'a self, key: &str) -> &'a str {
        self.find(key).map(|v| v.as_str()).unwrap_or("")
    }

    /// Finds the value corresponding to `key` and calls `as_vec()` on it.
    /// If the key does not exist, `vec!()` is returned.
    pub fn get_vec<'a>(&'a self, key: &str) -> Vec<&'a str> {
        self.find(key).map(|v| v.as_vec()).unwrap_or(vec!())
    }

    /// Return the raw value corresponding to some `key`.
    ///
    /// `key` should be a string in the traditional Docopt format. e.g.,
    /// `<arg>` or `--flag`.
    pub fn find<'a>(&'a self, key: &str) -> Option<&'a Value> {
        self.map.find(&key.to_string())
    }

    /// Return the number of values, not including synonyms.
    pub fn len(&self) -> uint {
        self.map.len()
    }

    /// Converts a Docopt key to a struct field name.
    /// This makes a half-hearted attempt at making the key a valid struct
    /// field name (like replacing `-` with `_`), but it does not otherwise
    /// guarantee that the result is a valid struct field name.
    #[doc(hidden)]
    pub fn key_to_struct_field(name: &str) -> String {
        fn sanitize(name: &str) -> String {
            name.replace("-", "_")
        }

        let r = regex!(r"^(?:--?(?P<flag>\S+)|(?:(?P<argu>\p{Lu}+)|<(?P<argb>[^>]+)>)|(?P<cmd>\S+))$");
        r.replace(name, |&: cap: &regex::Captures| {
            let (flag, cmd) = (
                cap.name("flag").unwrap_or(""),
                cap.name("cmd").unwrap_or(""),
            );
            let (argu, argb) = (
                cap.name("argu").unwrap_or(""),
                cap.name("argb").unwrap_or(""),
            );
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
                    panic!("Unknown ArgvMap key: '{}'", name)
                };
            let mut prefix = prefix.to_string();
            prefix.push_str(sanitize(name).as_slice());
            prefix
        })
    }

    /// Converts a struct field name to a Docopt key.
    #[doc(hidden)]
    pub fn struct_field_to_key(field: &str) -> String {
        fn desanitize(name: &str) -> String {
            name.replace("_", "-")
        }
        let name =
            if field.starts_with("flag_") {
                let name = regex!(r"^flag_").replace(field, "");
                let mut pre_name = (if name.len() == 1 { "-" } else { "--" })
                                   .to_string();
                pre_name.push_str(name.as_slice());
                pre_name
            } else if field.starts_with("arg_") {
                let name = regex!(r"^arg_").replace(field, "");
                if regex!(r"^\p{Lu}+$").is_match(name.as_slice()) {
                    name
                } else {
                    let mut pre_name = "<".to_string();
                    pre_name.push_str(name.as_slice());
                    pre_name.push('>');
                    pre_name
                }
            } else if field.starts_with("cmd_") {
                { regex!(r"^cmd_") }.replace(field, "")
            } else {
                panic!("Unrecognized struct field: '{}'", field)
            };
        desanitize(name.as_slice())
    }
}

impl fmt::Show for ArgvMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.len() == 0 {
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
            match reverse.get(&k) {
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

/// A matched command line value.
///
/// The value can be a boolean, counted repetition, a plain string or a list
/// of strings.
///
/// The various `as_{bool,count,str,vec}` methods provide convenient access
/// to values without destructuring manually.
#[derive(Clone, PartialEq, Show)]
pub enum Value {
    /// A boolean value from a flag that has no argument.
    ///
    /// The presence of a flag means `true` and the absence of a flag
    /// means `false`.
    Switch(bool),

    /// The number of occurrences of a repeated flag.
    Counted(uint),

    /// A positional or flag argument.
    ///
    /// This is `None` when the positional argument or flag is not present.
    /// Note that it is possible to have `Some("")` for a present but empty
    /// argument.
    Plain(Option<String>),

    /// A List of positional or flag arguments.
    ///
    /// This list may be empty when no arguments or flags are present.
    List(Vec<String>),
}

impl Value {
    /// Returns the value as a bool.
    ///
    /// Counted repetitions are `false` if `0` and `true` otherwise.
    /// Plain strings are `true` if present and `false` otherwise.
    /// Lists are `true` if non-empty and `false` otherwise.
    pub fn as_bool(&self) -> bool {
        match *self {
            Switch(b) => b,
            Counted(n) => n > 0,
            Plain(None) => false,
            Plain(Some(_)) => true,
            List(ref vs) => !vs.is_empty(),
        }
    }

    /// Returns the value as a count of the number of times it occurred.
    ///
    /// Booleans are `1` if `true` and `0` otherwise.
    /// Plain strings are `1` if present and `0` otherwise.
    /// Lists correspond to its length.
    pub fn as_count(&self) -> uint {
        match *self {
            Switch(b) => if b { 1 } else { 0 },
            Counted(n) => n,
            Plain(None) => 0,
            Plain(Some(_)) => 1,
            List(ref vs) => vs.len(),
        }
    }

    /// Returns the value as a string.
    ///
    /// All values return an empty string except for a non-empty plain string.
    pub fn as_str<'a>(&'a self) -> &'a str {
        match *self {
            Switch(_) | Counted(_) | Plain(None) | List(_) => "",
            Plain(Some(ref s)) => s.as_slice(),
        }
    }

    /// Returns the value as a list of strings.
    ///
    /// Booleans, repetitions and empty strings correspond to an empty list.
    /// Plain strings correspond to a list of length `1`.
    pub fn as_vec<'a>(&'a self) -> Vec<&'a str> {
        match *self {
            Switch(_) | Counted(_) | Plain(None) => vec!(),
            Plain(Some(ref s)) => vec!(s.as_slice()),
            List(ref vs) => vs.iter().map(|s| s.as_slice()).collect(),
        }
    }
}

/// Decoder for `ArgvMap` into your own `Decodable` types.
///
/// In general, you shouldn't have to use this type directly. It is exposed
/// in case you want to write a generic function that produces a decodable
/// value. For example, here's a function that takes a usage string, an argv
/// and produces a decodable value:
///
/// ```rust
/// # extern crate docopt;
/// # extern crate "rustc-serialize" as rustc_serialize;
/// # fn main() {
/// use docopt::Docopt;
/// use rustc_serialize::Decodable;
///
/// fn decode<D>(usage: &str, argv: &[&str]) -> Result<D, docopt::Error>
///          where D: Decodable<docopt::Decoder, docopt::Error> {
///     Docopt::new(usage)
///            .and_then(|d| d.argv(argv.iter().map(|&v|v)).decode())
/// }
/// # }
pub struct Decoder {
    vals: ArgvMap,
    stack: Vec<DecoderItem>,
}

#[derive(Show)]
struct DecoderItem {
    key: String,
    struct_field: String,
    val: Option<Value>,
}

macro_rules! derr(
    ($($arg:tt)*) => (return Err(Decode(format!($($arg)*))))
);

impl Decoder {
    fn push(&mut self, struct_field: &str) {
        let key = ArgvMap::struct_field_to_key(struct_field);
        self.stack.push(DecoderItem {
            key: key.clone(),
            struct_field: struct_field.to_string(),
            val: self.vals.find(key.as_slice()).map(|v| v.clone()),
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
                match v.as_str().parse() {
                    None => derr!("Could not decode '{}' to {} for '{}'.",
                                  v.as_str(), expect, k),
                    Some(v) => Ok(v),
                }
            }
        }
    }
}

impl rustc_serialize::Decoder<Error> for Decoder {
    fn error(&mut self, err: &str) -> Error {
        Decode(err.to_string())
    }
    fn read_nil(&mut self) -> Result<(), Error> {
        // I don't know what the right thing is here, so just fail for now.
        panic!("I don't know how to read into a nil value.")
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
        match vstr.chars().count() {
            1 => Ok(vstr.char_at(0)),
            _ => derr!("Could not decode '{}' into char for '{}'.", vstr, k),
        }
    }
    fn read_str(&mut self) -> Result<String, Error> {
        self.pop_val().map(|v| v.as_str().to_string())
    }
    fn read_enum<T, F>(&mut self, _: &str, f: F) -> Result<T, Error>
        where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        f(self)
    }
    fn read_enum_variant<T, F>(&mut self, names: &[&str], mut f: F)
                              -> Result<T, Error>
            where F: FnMut(&mut Decoder, uint) -> Result<T, Error> {
        fn to_lower(s: &str) -> String {
            s.chars().map(|c| c.to_lowercase()).collect()
        }

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
    fn read_enum_variant_arg<T, F>(&mut self, _: uint, _: F)
                                  -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_enum_struct_variant<T, F>(&mut self, _: &[&str], _: F)
                                     -> Result<T, Error>
            where F: FnMut(&mut Decoder, uint) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_enum_struct_variant_field<T, F>(&mut self, _: &str, _: uint, _: F)
                                           -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_struct<T, F>(&mut self, _: &str, _: uint, f: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        f(self)
    }
    fn read_struct_field<T, F>(&mut self, f_name: &str, _: uint, f: F)
                              -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        self.push(f_name);
        f(self)
    }
    fn read_tuple<T, F>(&mut self, _: uint, _: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_tuple_arg<T, F>(&mut self, _: uint, _: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_tuple_struct<T, F>(&mut self, _: &str, _: uint, _: F)
                              -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_tuple_struct_arg<T, F>(&mut self, _: uint, _: F)
                                  -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_option<T, F>(&mut self, mut f: F) -> Result<T, Error>
            where F: FnMut(&mut Decoder, bool) -> Result<T, Error> {
        let option =
            match self.stack.last() {
                None => derr!("Could not decode value into unknown key."),
                Some(it) => it.val.as_ref()
                                  .map(|v| v.as_bool())
                                  .unwrap_or(false),
            };
        f(self, option)
    }
    fn read_seq<T, F>(&mut self, f: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder, uint) -> Result<T, Error> {
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
    fn read_seq_elt<T, F>(&mut self, _: uint, f: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        f(self)
    }
    fn read_map<T, F>(&mut self, _: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder, uint) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_map_elt_key<T, F>(&mut self, _: uint, _: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
    fn read_map_elt_val<T, F>(&mut self, _: uint, _: F) -> Result<T, Error>
            where F: FnOnce(&mut Decoder) -> Result<T, Error> {
        unimplemented!()
    }
}

// I've been warned that this is wildly unsafe.
// Unless there's a reasonable alternative, I'm inclined to say that this is
// the price you pay for convenience.
fn exit(code: uint) -> ! {
    unsafe { libc::exit(code as libc::c_int) }
}

#[doc(hidden)]
pub mod parse;
mod synonym;
#[cfg(test)]
mod test;
