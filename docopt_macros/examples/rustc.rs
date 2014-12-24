#![feature(phase)]

extern crate "rustc-serialize" as rustc_serialize;

extern crate docopt;
#[phase(plugin)] extern crate docopt_macros;

docopt!(Args deriving Show, "
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
", flag_opt_level: Option<OptLevel>, flag_emit: Option<Emit>);

#[deriving(RustcDecodable, Show)]
enum Emit { Asm, Ir, Bc, Obj, Link }

#[deriving(Show)]
enum OptLevel { Zero, One, Two, Three }

impl<E, D> rustc_serialize::Decodable<D, E> for OptLevel
        where D: rustc_serialize::Decoder<E> {
    fn decode(d: &mut D) -> Result<OptLevel, E> {
        Ok(match try!(d.read_uint()) {
            0 => OptLevel::Zero, 1 => OptLevel::One,
            2 => OptLevel::Two, 3 => OptLevel::Three,
            n => {
                let err = format!("Could not decode '{}' as opt-level.", n);
                return Err(d.error(err.as_slice()));
            }
        })
    }
}

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    println!("{}", args);
}
