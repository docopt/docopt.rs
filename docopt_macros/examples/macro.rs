#![feature(phase)]

extern crate serialize;
#[phase(plugin)] extern crate docopt_macros;
extern crate docopt;

use docopt::FlagParser;

docopt!(Args, "
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
", arg_x: Option<int>, arg_y: Option<int>, flag_speed: int)

fn main() {
    let args: Args = FlagParser::parse().unwrap_or_else(|e| e.exit());
    println!("{}", args);

    println!("\nSome values:");
    println!("  Speed: {}", args.flag_speed);
    println!("  Drifting? {}", args.flag_drifting);
    println!("  Names: {}", args.arg_name);
}
