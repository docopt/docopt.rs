//! The one and only error type for docopt.
use std::error::Error as StdError;
use serde::de;
use std::fmt;
use std::result;
use std::io::{self, Write};

use crate::utils;

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
/// const USAGE: &'static str = "
/// Usage: ...
/// ";
///
/// let args = Docopt::new(USAGE)
///                   .and_then(|d| d.parse())
///                   .unwrap_or_else(|e| e.exit());
/// ```
#[derive(Debug)]
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

    /// This indicates a problem deserializing a successful argv match into a
    /// deserializable value.
    Deserialize(String),

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
        match self {
            Self::Help | Self::Version(..) => false,
            Self::Usage(..) | Self::Argv(..) | Self::NoMatch | Self::Deserialize(..) => true,
            Self::WithProgramUsage(ref b, _) => b.fatal(),
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
            ::std::process::exit(1)
        } else {
            let _ = writeln!(&mut io::stdout(), "{}", self);
            ::std::process::exit(0)
        }
    }
}

/// Short result type alias.
pub type Result<T> = result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WithProgramUsage(ref other, ref usage) => {
                let other = other.to_string();
                if other.is_empty() {
                    write!(f, "{}", usage)
                } else {
                    write!(f, "{}\n\n{}", other, usage)
                }
            }
            Self::Help => write!(f, ""),
            Self::NoMatch => write!(f, "Invalid arguments."),
            Self::Usage(ref s) |
            Self::Argv(ref s) |
            Self::Deserialize(ref s) |
            Self::Version(ref s) => write!(f, "{}", s),
        }
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::WithProgramUsage(ref cause, _) => Some(&**cause),
            _ => None,
        }
    }
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Self::Deserialize(msg.to_string())
    }
}
