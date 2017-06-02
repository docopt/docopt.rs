#![feature(plugin)]
#![plugin(docopt_macros)]

#[macro_use]
extern crate serde_derive;

extern crate docopt;

docopt!(Args derive Debug, "
Usage: cp [options] <src> <dst>
       cp [options] <src>... <dir>
       cp --help

Options:
  -h, --help       Show this message.
  -a, --archive    Copy everything.
");

fn main() {
    let args: Args = Args::docopt().deserialize().unwrap_or_else(|e| e.exit());
    println!("{:?}", args);
}
