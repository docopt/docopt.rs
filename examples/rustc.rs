#![feature(phase)]
extern crate serialize;
#[phase(plugin)] extern crate docopt_macros;
extern crate docopt;

use docopt::FlagParser;

docopt!(Args, "
Usage: rustc [options] [--cfg SPEC... -L PATH...] INPUT
       rustc (--help | --version)

Options:
    -h, --help         Show this message.
    --version          Show the version of rustc.
    --cfg SPEC         Configure the compilation environment.
    -L PATH            Add a directory to the library search path.
    --emit TYPE        Configure the output that rustc will produce.
                       Valid values: asm, ir, bc, obj, link.
    --opt-level LEVEL  Optimize with possible levels 0-3.
", flag_opt_level: Option<OptLevel>, flag_emit: Option<Emit>)

#[deriving(Decodable, Show)]
enum Emit { Asm, Ir, Bc, Obj, Link }

#[deriving(Show)]
enum OptLevel { Zero, One, Two, Three }

impl<E, D: serialize::Decoder<E>> serialize::Decodable<D, E> for OptLevel {
    fn decode(d: &mut D) -> Result<OptLevel, E> {
        Ok(match try!(d.read_uint()) {
            0 => Zero, 1 => One, 2 => Two, 3 => Three,
            _ => fail!("How to CONVENIENTLY create value with type `E`?"),
        })
    }
}

fn main() {
    let args: Args = FlagParser::parse();
    println!("{}", args);
}
