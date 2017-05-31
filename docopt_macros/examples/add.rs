#![feature(plugin)]
#![plugin(docopt_macros)]

#[macro_use]
extern crate serde_derive;

extern crate docopt;

docopt!(Args, "Usage: add <x> <y>", arg_x: usize, arg_y: usize);

fn main() {
    let args: Args = Args::docopt().deserialize().unwrap_or_else(|e| e.exit());
    println!("x: {}, y: {}", args.arg_x, args.arg_y);
}
