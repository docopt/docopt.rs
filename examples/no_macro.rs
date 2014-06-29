extern crate docopt;

use docopt::docopt;

static docstr: &'static str = "
Naval Fate.

Usage:
  naval_fate.py ship new <name>...
  naval_fate.py ship <name> move <x> <y> [--speed=<kn>]
  naval_fate.py ship shoot <x> <y>
  naval_fate.py mine (set|remove) <x> <y> [--moored | --drifting]
  naval_fate.py (-h | --help)
  naval_fate.py --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.
";

fn main() {
    let args = docopt(docstr);
    // If you invoke with:
    //
    //     no_macro ship Guardian move 100 150 --speed=15
    //
    // then you should get the following map:
    //
    //     --drifting  => Switch(false)
    //     -h, --help  => Switch(false)
    //     --moored    => Switch(false) 
    //     --speed     => Plain(Some(15)) 
    //     --version   => Switch(false) 
    //     <name>      => List([Guardian]) 
    //     <x>         => Plain(Some(100)) 
    //     <y>         => Plain(Some(150)) 
    //     mine        => Switch(false) 
    //     move        => Switch(true) 
    //     new         => Switch(false) 
    //     remove      => Switch(false) 
    //     set         => Switch(false) 
    //     ship        => Switch(true) 
    //     shoot       => Switch(false) 
    println!("{}", args);

    // You can conveniently access values with `get_{bool,count,str,vec}`
    // functions. If the key doesn't exist (or if, e.g., you use `get_str` on
    // a switch), then a sensible default value is returned.
    println!("\nSome values:");
    println!("  Speed: {}", args.get_str("--speed"));
    println!("  Drifting? {}", args.get_bool("--drifting"));
    println!("  Names: {}", args.get_vec("<name>"));
}
