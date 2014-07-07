#![feature(phase)]

#[phase(plugin, link)] extern crate docopt;
extern crate serialize;

docopt!(Args, "
Usage: cp [options] SOURCE DEST
       cp [options] SOURCE... DIR
       cp --help

Options:
  -h, --help       Show this message.
  -a, --archive    Copy everything.
")

fn main() {
    let args = Args::parse();
    println!("{}", args);
}
