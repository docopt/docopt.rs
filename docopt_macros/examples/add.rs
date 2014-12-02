#![feature(phase)]

extern crate serialize;

extern crate docopt;
#[phase(plugin)] extern crate docopt_macros;

docopt!(Args, "Usage: add <x> <y>", arg_x: int, arg_y: int)

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    println!("x: {}, y: {}", args.arg_x, args.arg_y);
}
