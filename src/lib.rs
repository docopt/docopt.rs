#![crate_id = "docopt#0.1.0"]
#![crate_type = "rlib"]
#![crate_type = "dylib"]
#![license = "UNLICENSE"]
#![doc(html_root_url = "http://burntsushi.net/rustdoc/docopt")]

#![allow(dead_code, unused_variable, unused_imports, visible_private_types)]

//! Docopt for Rust.

#![feature(macro_rules, phase)]

extern crate debug;
extern crate regex;
#[phase(plugin)] extern crate regex_macros;

pub use parse::Docopt;

mod parse;
mod synonym;
#[cfg(test)]
mod test;

