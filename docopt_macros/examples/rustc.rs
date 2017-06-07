#![feature(plugin)]
#![plugin(docopt_macros)]

#[macro_use]
extern crate serde_derive;
extern crate serde;

extern crate docopt;

use serde::de;

docopt!(Args derive Debug, "
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

#[derive(Debug, Deserialize)]
enum Emit {
    Asm,
    Ir,
    Bc,
    Obj,
    Link,
}

#[derive(Debug)]
enum OptLevel {
    Zero,
    One,
    Two,
    Three,
}

impl<'de> de::Deserialize<'de> for OptLevel {
    fn deserialize<D>(deserializer: D) -> Result<OptLevel, D::Error>
        where D: de::Deserializer<'de>
    {
        let level = match u8::deserialize(deserializer)? {
            0 => OptLevel::Zero,
            1 => OptLevel::One,
            2 => OptLevel::Two,
            3 => OptLevel::Three,
            n => {
                let value = de::Unexpected::Unsigned(n as u64);
                let msg = "expected an integer between 0 and 3";
                return Err(de::Error::invalid_value(value, &msg));
            }
        };
        Ok(level)
    }
}

fn main() {
    let args: Args = Args::docopt().deserialize().unwrap_or_else(|e| e.exit());
    println!("{:?}", args);
}
