#![feature(plugin)]
#![plugin(docopt_macros)]

#[macro_use]
extern crate serde_derive;

extern crate docopt;

docopt!(pub Args derive Debug, "
Naval Fate.

Usage:
  naval_fate.py ship new <name>...
  naval_fate.py ship <name> move <x> <y> [--speed=<kn>]
  naval_fate.py ship shoot <x> <y>
  naval_fate.py mine (set|remove) <x> <y> [--moored | --drifting]
  naval_fate.py --help
  naval_fate.py --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.
", arg_x: Option<usize>, arg_y: Option<usize>, flag_speed: usize);

fn main() {
    let args: Args = Args::docopt().deserialize().unwrap_or_else(|e| e.exit());
    println!("{:?}", args);

    println!("\nSome values:");
    println!("  Speed: {}", args.flag_speed);
    println!("  Drifting? {}", args.flag_drifting);
    println!("  Names: {:?}", args.arg_name);
}
