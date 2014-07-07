#![feature(phase)]

extern crate serialize;
#[phase(plugin, link)] extern crate docopt;

docopt!(Args, "Usage: add <x> <y>", arg_x: int, arg_y: int)

fn main() {
    let args = Args::parse();
    println!("x: {:d}, y: {:d}", args.arg_x, args.arg_y);
}
