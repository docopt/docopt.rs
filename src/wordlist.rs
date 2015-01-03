#![experimental]
#![feature(macro_rules, old_orphan_check)]

extern crate regex;
extern crate "rustc-serialize" as rustc_serialize;

extern crate docopt;

use std::collections::HashMap;
use std::io;
use std::os;

pub use docopt::{Docopt, Value};

use parse::{Atom, Parser};

// cheat until we get syntax extensions back :-(
macro_rules! regex(
    ($s:expr) => (regex::Regex::new($s).unwrap());
);

#[allow(dead_code)]
mod parse;
#[allow(dead_code)]
mod synonym;

static USAGE: &'static str = "
Usage: docopt-wordlist [(<name> <possibles>)] ...

docopt-wordlist prints a list of available flags and commands arguments for the
given usage (provided on stdin).

Example use:

  your-command --help | docopt-wordlist

This command also supports completing positional arguments when given a list of
choices. The choices are included in the word list if and only if the argument
name appears in the usage string. For example:

  your-command --help | docopt-wordlist 'arg' 'a b c'

Which will only include 'a', 'b' and 'c' in the wordlist if
'your-command --help' contains a positional argument named 'arg'.
";

#[derive(RustcDecodable, Show)]
struct Args {
    arg_name: Vec<String>,
    arg_possibles: Vec<String>,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.decode())
                            .unwrap_or_else(|e| e.exit());
    match run(args) {
        Ok(_) => {},
        Err(err) => {
            os::set_exit_status(1);
            io::stderr().write_str(err.as_slice()).unwrap();
        }
    }
}

fn run(args: Args) -> Result<(), String> {
    let usage = try!(io::stdin().read_to_string().map_err(|e| e.to_string()));
    let parsed = try!(Parser::new(usage.as_slice())
                             .map_err(|e| e.to_string()));
    let arg_possibles: HashMap<String, Vec<String>> =
        args.arg_name.iter()
                     .zip(args.arg_possibles.iter())
                     .map(|(name, possibles)| {
                         let choices =
                             regex!(r"[ \t]+").split(possibles.as_slice())
                                              .map(|s| s.to_string())
                                              .collect::<Vec<String>>();
                         (name.clone(), choices)
                     })
                     .collect();

    let mut words = vec![];
    for k in parsed.descs.keys() {
        if let Atom::Positional(ref arg_name) = *k {
            if let Some(choices) = arg_possibles.get(arg_name) {
                words.extend(choices.iter().map(|s| s.clone()));
            }
            // If the user hasn't given choices for this positional argument,
            // then there's really nothing to complete here.
        } else {
            words.push(k.to_string());
        }
    }
    for (k, _) in parsed.descs.synonyms() {
        // We don't need to do anything special here since synonyms can
        // only be flags, which we always include in the wordlist.
        words.push(k.to_string());
    }
    println!("{}", words.connect(" "));
    Ok(())
}
