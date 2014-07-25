#![feature(phase)]

extern crate serialize;
#[phase(plugin)] extern crate docopt_macros;
extern crate docopt;

use docopt::FlagParser;

docopt!(Args, "Usage: add <x> <y>", arg_x: int, arg_y: int)

fn main() {
    let args: Args = FlagParser::parse().unwrap();
    println!("x: {:d}, y: {:d}", args.arg_x, args.arg_y);
}
