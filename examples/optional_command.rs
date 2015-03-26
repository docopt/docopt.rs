// This example shows how to implement a command with a "catch all."
//
// This requires writing your own impl for `Decodable` because docopt's
// decoder uses `Option<T>` to mean "T may not be present" rather than
// "T may be present but incorrect."

extern crate rustc_serialize;
extern crate docopt;

use docopt::Docopt;
use rustc_serialize::{Decodable, Decoder};

// Write the Docopt usage string.
static USAGE: &'static str = "
Rust's package manager

Usage:
    mycli [<command>]

Options:
    -h, --help       Display this message
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_command: Command,
}

impl Decodable for Command {
    fn decode<D: Decoder>(d: &mut D) -> Result<Command, D::Error> {
        let s = try!(d.read_str());
        Ok(match &*s {
            "" => Command::None,
            "A" => Command::A,
            "B" => Command::B,
            "C" => Command::C,
            s => Command::Unknown(s.to_string()),
        })
    }
}

#[derive(Debug)]
enum Command { A, B, C, Unknown(String), None }

fn main() {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.decode())
                            .unwrap_or_else(|e| e.exit());
    println!("{:?}", args);
}
