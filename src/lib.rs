#![crate_id = "docopt#0.1.0"]
#![crate_type = "rlib"]
#![crate_type = "dylib"]
#![license = "UNLICENSE"]
#![doc(html_root_url = "http://burntsushi.net/rustdoc/docopt")]

#![allow(dead_code, unused_variable, unused_imports)]
#![allow(visible_private_types)]

//! Docopt for Rust.

#![feature(macro_rules, phase)]

extern crate debug;
extern crate libc;
// #[cfg(test)] 
#[phase(plugin, link)]
extern crate log;
extern crate regex;
#[phase(plugin)] extern crate regex_macros;
extern crate serialize;

use std::collections::HashMap;
use std::fmt;
use std::from_str::{FromStr, from_str};
use std::num;
use serialize::Decodable;
use parse::Parser;
use synonym::SynonymMap;

macro_rules! werr(
    ($($arg:tt)*) => (
        match io::stderr().write_str(format!($($arg)*).as_slice()) {
            Ok(_) => (),
            Err(err) => fail!("{}", err),
        }
    )
)

pub enum Error {
    NoMatch,
    Usage(String),
}

impl fmt::Show for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &NoMatch => write!(f, "Invalid arguments."),
            &Usage(ref s) => write!(f, "{}", s),
        }
    }
}

#[deriving(Show)]
pub struct Docopt {
    p: Parser,
    conf: Config,
}

#[deriving(Clone, Show)]
pub struct Config {
    pub options_first: bool,
    pub help: bool,
    pub version: Option<String>,
}

impl Docopt {
    pub fn new(doc: &str, conf: Config) -> Result<Docopt, Error> {
        Parser::new(doc, conf.options_first)
            .map(|p| Docopt { p: p, conf: conf.clone() })
            .or_else(|s| Err(Usage(s)))
    }

    pub fn argv(&self, args: &[&str]) -> Result<ValueMap, Error> {
        self.p.parse_argv(args)
            .or_else(|s| Err(Usage(s)))
            .and_then(|argv|
                self.p.matches(&argv)
                    .map(|m| Ok(ValueMap { map: m }))
                    .unwrap_or_else(|| Err(NoMatch)))
    }
}

#[deriving(Clone)]
pub struct ValueMap {
    map: SynonymMap<String, Value>,
}

#[deriving(Clone, PartialEq, Show)]
pub enum Value {
    Switch(bool),
    Counted(uint),
    Plain(Option<String>),
    List(Vec<String>),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            &Switch(b) => b,
            &Counted(n) => n > 0,
            &Plain(None) => false,
            &Plain(Some(_)) => true,
            &List(ref vs) => !vs.is_empty(),
        }
    }
    pub fn as_count(&self) -> uint {
        match self {
            &Switch(b) => if b { 1 } else { 0 },
            &Counted(n) => n,
            &Plain(None) => 0,
            &Plain(Some(_)) => 1,
            &List(ref vs) => vs.len(),
        }
    }
    pub fn as_str<'a>(&'a self) -> &'a str {
        match self {
            &Switch(_) | &Counted(_) | &Plain(None) | &List(_) => "",
            &Plain(Some(ref s)) => s.as_slice(),
        }
    }
    pub fn as_vec<'a>(&'a self) -> Vec<&'a str> {
        match self {
            &Switch(_) | &Counted(_) | &Plain(None) => vec!(),
            &Plain(Some(ref s)) => vec!(s.as_slice()),
            &List(ref vs) => vs.iter().map(|s| s.as_slice()).collect(),
        }
    }
}

impl ValueMap {
    pub fn decode<'a, T: Decodable<Decoder<'a>, DecoderError>>
                 (&'a self) -> Result<T, DecoderError> {
        Decodable::decode(&mut Decoder { vals: self, stack: vec!() })
    }
    pub fn get_bool(&self, key: &str) -> bool {
        self.find(&key).map(|v| v.as_bool()).unwrap_or(false)
    }
    pub fn get_count(&self, key: &str) -> uint {
        self.find(&key).map(|v| v.as_count()).unwrap_or(0)
    }
    pub fn get_str<'a>(&'a self, key: &str) -> &'a str {
        self.find(&key).map(|v| v.as_str()).unwrap_or("")
    }
    pub fn get_vec<'a>(&'a self, key: &str) -> Vec<&'a str> {
        self.find(&key).map(|v| v.as_vec()).unwrap_or(vec!())
    }

    fn key_to_struct_field(name: &str) -> String {
        fn sanitize(name: &str) -> String {
            name.replace("-", "_")
        }

        let r = regex!(r"--?(?P<flag>[^-]+)|(?:(?P<argu>\p{Lu})|<(?P<argb>[^>]+)>)|(?P<cmd>\S+)");
        r.replace(name, |cap: &regex::Captures| {
            let (flag, cmd) = (cap.name("flag"), cap.name("cmd"));
            let (argu, argb) = (cap.name("argu"), cap.name("argb"));
            let (prefix, name) =
                if !flag.is_empty() {
                    ("flag_", flag)
                } else if !argu.is_empty() {
                    ("arg_", argu)
                } else if !argb.is_empty() {
                    ("arg_", argb)
                } else if !cmd.is_empty() {
                    ("cmd_", cmd)
                } else {
                    fail!("Unknown ValueMap key: '{}'", name)
                };
            prefix.to_string().append(sanitize(name).as_slice())
        })
    }

    fn struct_field_to_key(field: &str) -> String {
        fn desanitize(name: &str) -> String {
            name.replace("_", "-")
        }
        let name =
            if field.starts_with("flag_") {
                let name = regex!(r"^flag_").replace(field, "");
                if name.len() == 1 {
                    "-".to_string().append(name.as_slice())
                } else {
                    "--".to_string().append(name.as_slice())
                }
            } else if field.starts_with("arg_") {
                let name = regex!(r"^arg_").replace(field, "");
                if regex!(r"^\p{Lu}+$").is_match(name.as_slice()) {
                    name
                } else {
                    "<".to_string().append(name.as_slice()).append(">")
                }
            } else if field.starts_with("cmd_") {
                { regex!(r"^cmd_") }.replace(field, "")
            } else {
                fail!("Unrecognized struct field: '{}'", field)
            };
        desanitize(name.as_slice())
    }
}

impl Collection for ValueMap {
    fn len(&self) -> uint { self.map.len() }
}

impl<'k> Map<&'k str, Value> for ValueMap {
    fn find<'a>(&'a self, key: & &'k str) -> Option<&'a Value> {
        self.map.find(&key.to_string())
    }
}

impl fmt::Show for ValueMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_empty() {
            return write!(f, "{{EMPTY}}");
        }

        // This is a little crazy, but we want to group synonyms with
        // their keys and sort them for predictable output.
        let reverse: HashMap<&String, &String> =
            self.map.synonyms().map(|(from, to)| (to, from)).collect();
        let mut keys: Vec<&String> = self.map.keys().collect();
        keys.sort();
        let mut first = true;
        for &k in keys.iter() {
            if !first { try!(write!(f, "\n")); } else { first = false; }
            match reverse.find(&k) {
                None => {
                    try!(write!(f, "{} => {}", k, self.map.get(k)))
                }
                Some(s) => {
                    try!(write!(f, "{}, {} => {}", s, k, self.map.get(k)))
                }
            }
        }
        Ok(())
    }
}

struct Decoder<'a> {
    vals: &'a ValueMap,
    stack: Vec<DecoderItem>,
}

#[deriving(Show)]
struct DecoderItem {
    key: String,
    struct_field: String,
    val: Option<Value>,
}

pub type DecoderError = String;

impl<'a> Decoder<'a> {
    fn push(&mut self, struct_field: &str) {
        let key = ValueMap::struct_field_to_key(struct_field);
        self.stack.push(DecoderItem {
            key: key.clone(),
            struct_field: struct_field.to_string(),
            val: self.vals.find(&key.as_slice()).map(|v| v.clone()),
        });
    }

    fn pop(&mut self) -> Result<DecoderItem, DecoderError> {
        match self.stack.pop() {
            None => Err(format!("Could not decode value into unknown key.")),
            Some(it) => Ok(it),
        }
    }

    fn pop_key_val(&mut self) -> Result<(String, Value), DecoderError> {
        let it = try!(self.pop());
        match it.val {
            None => Err(format!("Could not find argument '{}' (from struct \
                                 field '{}').", it.key, it.struct_field)),
            Some(v) => Ok((it.key, v)),
        }
    }

    fn pop_val(&mut self) -> Result<Value, DecoderError> {
        let (_, v) = try!(self.pop_key_val());
        Ok(v)
    }

    fn to_number<T: FromStr + NumCast>
                (&mut self, expect: &str) -> Result<T, DecoderError> {
        let (k, v) = try!(self.pop_key_val());
        match v {
            Counted(n) => Ok(num::cast(n).unwrap()),
            _ => {
                match from_str(v.as_str()) {
                    None => Err(format!(
                                "Could not decode '{}' from string \
                                 to {} for '{}'.",
                                v.as_str(), expect, k)),
                    Some(v) => Ok(v),
                }
            }
        }
    }
}

impl<'a> serialize::Decoder<DecoderError> for Decoder<'a> {
    fn read_nil(&mut self) -> Result<(), DecoderError> {
        // I don't know what the right thing is here, so just fail for now.
        fail!("I don't know how to read into a nil value.")
    }
    fn read_uint(&mut self) -> Result<uint, DecoderError> {
        self.to_number("uint")
    }
    fn read_u64(&mut self) -> Result<u64, DecoderError> {
        self.to_number("u64")
    }
    fn read_u32(&mut self) -> Result<u32, DecoderError> {
        self.to_number("u32")
    }
    fn read_u16(&mut self) -> Result<u16, DecoderError> {
        self.to_number("u16")
    }
    fn read_u8(&mut self) -> Result<u8, DecoderError> {
        self.to_number("u8")
    }
    fn read_int(&mut self) -> Result<int, DecoderError> {
        self.to_number("int")
    }
    fn read_i64(&mut self) -> Result<i64, DecoderError> {
        self.to_number("i64")
    }
    fn read_i32(&mut self) -> Result<i32, DecoderError> {
        self.to_number("i32")
    }
    fn read_i16(&mut self) -> Result<i16, DecoderError> {
        self.to_number("i16")
    }
    fn read_i8(&mut self) -> Result<i8, DecoderError> {
        self.to_number("i8")
    }
    fn read_bool(&mut self) -> Result<bool, DecoderError> {
        self.pop_val().map(|v| v.as_bool())
    }
    fn read_f64(&mut self) -> Result<f64, DecoderError> {
        self.to_number("f64")
    }
    fn read_f32(&mut self) -> Result<f32, DecoderError> {
        self.to_number("f32")
    }
    fn read_char(&mut self) -> Result<char, DecoderError> {
        let (k, v) = try!(self.pop_key_val());
        let vstr = v.as_str();
        match vstr.len() {
            1 => Ok(vstr.char_at(0)),
            _ => Err(format!("Could not decode '{}' into char for '{}'.",
                             vstr, k)),
        }
    }
    fn read_str(&mut self) -> Result<String, DecoderError> {
        self.pop_val().map(|v| v.as_str().to_string())
    }
    fn read_enum<T>(&mut self, name: &str,
                    f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                    -> Result<T, DecoderError> {
        f(self)
    }
    fn read_enum_variant<T>(&mut self, names: &[&str],
                            f: |&mut Decoder<'a>, uint| -> Result<T, DecoderError>)
                            -> Result<T, DecoderError> {
        let v = try!(self.pop_val());
        let vstr = to_lower(v.as_str());
        let i =
            match names.iter().map(|&n| to_lower(n)).position(|n| n == vstr) {
                Some(i) => i,
                None => {
                    return Err(format!("Could not match '{}' with any of \
                                        the allowed variants: {}",
                                       vstr, names));
                }
            };
        f(self, i)
    }
    fn read_enum_variant_arg<T>(
        &mut self, a_idx: uint,
        f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
        -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_enum_struct_variant<T>(
        &mut self, names: &[&str],
        f: |&mut Decoder<'a>, uint| -> Result<T, DecoderError>)
        -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_enum_struct_variant_field<T>(
        &mut self, f_name: &str, f_idx: uint,
        f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
        -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_struct<T>(&mut self, s_name: &str, len: uint,
                      f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                      -> Result<T, DecoderError> {
        f(self)
    }
    fn read_struct_field<T>(&mut self, f_name: &str, f_idx: uint,
                            f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                            -> Result<T, DecoderError> {
        self.push(f_name);
        f(self)
    }
    fn read_tuple<T>(&mut self,
                     f: |&mut Decoder<'a>, uint| -> Result<T, DecoderError>)
                     -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_tuple_arg<T>(&mut self, a_idx: uint,
                         f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                         -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_tuple_struct<T>(&mut self, s_name: &str,
                            f: |&mut Decoder<'a>, uint| -> Result<T, DecoderError>)
                            -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_tuple_struct_arg<T>(&mut self, a_idx: uint,
                                f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                                -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_option<T>(&mut self,
                      f: |&mut Decoder<'a>, bool| -> Result<T, DecoderError>)
                      -> Result<T, DecoderError> {
        let option =
            match self.stack.last() {
                None => return Err(format!("Could not decode value into \
                                            unknown key.")),
                Some(it) => it.val.as_ref()
                                  .map(|v| v.as_bool())
                                  .unwrap_or(false),
            };
        f(self, option)
    }
    fn read_seq<T>(&mut self,
                   f: |&mut Decoder<'a>, uint| -> Result<T, DecoderError>)
                   -> Result<T, DecoderError> {
        let it = try!(self.pop());
        let list = it.val.unwrap_or(List(vec!()));
        let vals = list.as_vec();
        for val in vals.iter().rev() {
            self.stack.push(DecoderItem {
                key: it.key.clone(),
                struct_field: it.struct_field.clone(),
                val: Some(Plain(Some(val.to_string()))),
            })
        }
        f(self, vals.len())
    }
    fn read_seq_elt<T>(&mut self, idx: uint,
                       f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                       -> Result<T, DecoderError> {
        f(self)
    }
    fn read_map<T>(&mut self,
                   f: |&mut Decoder<'a>, uint| -> Result<T, DecoderError>)
                   -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_map_elt_key<T>(&mut self, idx: uint,
                           f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                           -> Result<T, DecoderError> {
        unimplemented!()
    }
    fn read_map_elt_val<T>(&mut self, idx: uint,
                           f: |&mut Decoder<'a>| -> Result<T, DecoderError>)
                           -> Result<T, DecoderError> {
        unimplemented!()
    }
}

pub fn docopt(doc: &str) -> ValueMap {
    docopt_conf(doc, Config { options_first: false, help: true, version: None })
}

pub fn docopt_conf(doc: &str, conf: Config) -> ValueMap {
    match Docopt::new(doc, conf) {
        Ok(dopt) => convenient_parse_argv(&dopt),
        Err(err) => fail!("{}", err),
    }
}

fn convenient_parse_argv(dopt: &Docopt) -> ValueMap {
    use std::io;
    use std::os;

    let os_argv = os::args();
    let argv: Vec<&str> = os_argv.iter().skip(1).map(|s|s.as_slice()).collect();
    if dopt.conf.help && argv_has_help(argv.as_slice()) {
        werr!("{}\n", dopt.p.full_doc.as_slice().trim());
        exit(1);
    }
    match dopt.conf.version {
        Some(ref v) if argv_has_version(argv.as_slice()) => {
            werr!("{}\n", v);
            exit(0);
        }
        _ => {},
    }
    dopt.argv(argv.as_slice()).unwrap_or_else(|err| {
        werr!("{}\n", err);
        werr!("{}\n", dopt.p.usage.as_slice().trim());
        exit(1);
    })
}

fn argv_has_help(argv: &[&str]) -> bool {
    argv.contains(&"-h") || argv.contains(&"--help")
}

fn argv_has_version(argv: &[&str]) -> bool {
    argv.contains(&"--version")
}

fn to_lower(s: &str) -> String {
    s.chars().map(|c| c.to_lowercase()).collect()
}

// I've been warned that this is wildly unsafe.
// Unless there's a reasonable alternative, I'm inclined to say that this is
// the price you pay for convenience.
fn exit(code: uint) -> ! {
    unsafe { libc::exit(code as libc::c_int) }
}

mod parse;
mod synonym;
#[cfg(test)]
mod test;

