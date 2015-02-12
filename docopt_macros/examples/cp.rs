#![feature(plugin)]
#![plugin(docopt_macros)]

extern crate "rustc-serialize" as rustc_serialize;

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
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    println!("{:?}", args);
}
