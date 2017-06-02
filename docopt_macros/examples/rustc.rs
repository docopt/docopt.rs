#![feature(plugin)]
#![plugin(docopt_macros)]

#[macro_use]
extern crate serde_derive;
extern crate serde;

extern crate docopt;

use serde::de;
use std::fmt;

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

struct OptLevelVisitor;

impl<'de> de::Visitor<'de> for OptLevelVisitor {
    type Value = OptLevel;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("an integer between 0 and 3")
    }

    fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
        where E: de::Error
    {
        let level = match value {
            0 => OptLevel::Zero,
            1 => OptLevel::One,
            2 => OptLevel::Two,
            3 => OptLevel::Three,
            n => {
                let err = format!("Could not deserialize '{}' as opt-level.", n);
                return Err(E::custom(err));
            }
        };
        Ok(level)
    }
}

impl<'de> de::Deserialize<'de> for OptLevel {
    fn deserialize<D>(deserializer: D) -> Result<OptLevel, D::Error>
        where D: de::Deserializer<'de>
    {
        deserializer.deserialize_u8(OptLevelVisitor)
    }
}

fn main() {
    let args: Args = Args::docopt().deserialize().unwrap_or_else(|e| e.exit());
    println!("{:?}", args);
}
