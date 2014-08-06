#![feature(phase)]

#[phase(plugin)] extern crate docopt_macros;
extern crate docopt;
extern crate serialize;

use docopt::FlagParser;

docopt!(Args, "
Usage: cp [options] SOURCE DEST
       cp [options] SOURCE... DIR
       cp --help

Options:
  -h, --help       Show this message.
  -a, --archive    Copy everything.
")

fn main() {
    let args: Args = FlagParser::parse().unwrap_or_else(|e| e.exit());
    println!("{}", args);
}
