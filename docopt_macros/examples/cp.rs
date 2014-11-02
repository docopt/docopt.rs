#![feature(phase)]

extern crate serialize;

extern crate docopt;
#[phase(plugin)] extern crate docopt_macros;

docopt!(Args deriving Show, "
Usage: cp [options] <src> <dst>
       cp [options] <src>... <dir>
       cp --help

Options:
  -h, --help       Show this message.
  -a, --archive    Copy everything.
")

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    println!("{}", args);
}
