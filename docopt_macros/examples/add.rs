#![feature(plugin)]

extern crate "rustc-serialize" as rustc_serialize;

extern crate docopt;
#[plugin] extern crate docopt_macros;

docopt!(Args, "Usage: add <x> <y>", arg_x: usize, arg_y: usize);

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    println!("x: {}, y: {}", args.arg_x, args.arg_y);
}
