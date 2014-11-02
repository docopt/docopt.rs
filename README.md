Docopt for Rust with automatic type based decoding (i.e., data validation).
This implementation conforms to the 
[official description of Docopt](http://docopt.org/) and
[passes its test suite](https://github.com/docopt/docopt/pull/201).

[![Build status](https://api.travis-ci.org/docopt/docopt.rs.svg)](https://travis-ci.org/docopt/docopt.rs)

Licensed under the [UNLICENSE](http://unlicense.org).


### Current status
Fully functional but the design of the API is up for debate. **I am seeking 
feedback**.


### Documentation

[http://burntsushi.net/rustdoc/docopt](http://burntsushi.net/rustdoc/docopt/index.html)


### Installation

This crate is fully compatible with Cargo. Just add it to your `Cargo.toml`:

```toml
[dependencies.docopt_macros]
git = "git://github.com/docopt/docopt.rs"
```

If you don't want to use the macro, then you can change your entry to
`dependencies.docopt`.


### Quick example

Here is a full working example:

```rust
extern crate serialize;
extern crate docopt;

use docopt::Docopt;

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: cp [-a] <source> <dest>
       cp [-a] <source>... <dir>

Options:
    -a, --archive  Copy everything.
";

#[deriving(Decodable, Show)]
struct Args {
    arg_source: Vec<String>,
    arg_dest: String,
    arg_dir: String,
    flag_archive: bool,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.decode())
                            .unwrap_or_else(|e| e.exit());
    println!("{}", args);
}
```

Here is the same example, but with the use of the `docopt!` macro, which will
*generate a struct for you*:

```rust
#![feature(phase)]

extern crate serialize;

extern crate docopt;
#[phase(plugin)] extern crate docopt_macros;

docopt!(Args deriving Show, "
Usage: cp [options] <src> <dst>
       cp [options] <src>... <dir>
       cp --help

Options:
  -h, --help       Show this message.
  -a, --archive    Copy everything.
")

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    println!("{}", args);
}
```

The field names of the struct map like this:

```
-g       => flag_g
--group  => flag_group
FILE     => arg_FILE
<file>   => arg_file
build    => cmd_build
```

The `Args` struct has one static method defined for it: `docopt`. The method
returns a normal `Docopt` value, which can be used to set configuration
options, `argv` and parse or decode command line arguments.


### Data validation example

Here's another example that shows how to specify the types of your arguments:

```rust
#![feature(phase)]

extern crate serialize;

extern crate docopt;
#[phase(plugin)] extern crate docopt_macros;

docopt!(Args, "Usage: add <x> <y>", arg_x: int, arg_y: int)

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());
    println!("x: {:d}, y: {:d}", args.arg_x, args.arg_y);
}
```

In this example, specific type annotations were added. They will be 
automatically inserted into the generated struct. You can override as many (or 
as few) fields as you want. If you don't specify a type, then one of `bool`, 
`uint`, `String` or `Vec<String>` will be chosen depending on the type of 
argument. In this case, both `arg_x` and `arg_y` would have been `String`.

If any value cannot be decoded into a value with the right type, then an error 
will be shown to the user.

And of course, you don't need the macro to do this. You can do the same thing
with a manually written struct too.


### Modeling `rustc`

Here's a selected subset for some of `rustc`'s options. This also shows how to 
restrict values to a list of choices via an `enum` type and demonstrates more
Docopt features.

```rust
#![feature(phase)]

extern crate serialize;

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
", flag_opt_level: Option<OptLevel>, flag_emit: Option<Emit>)

#[deriving(Decodable, Show)]
enum Emit { Asm, Ir, Bc, Obj, Link }

#[deriving(Show)]
enum OptLevel { Zero, One, Two, Three }

impl<E, D: serialize::Decoder<E>> serialize::Decodable<D, E> for OptLevel {
    fn decode(d: &mut D) -> Result<OptLevel, E> {
        Ok(match try!(d.read_uint()) {
            0 => Zero, 1 => One, 2 => Two, 3 => Three,
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
```

### Viewing the generated struct

Generating a struct is pretty magical, but if you want, you can look at it by 
expanding all macros. Say you wrote the above example for `Usage: add <x> <y>`
into a file called `add.rs`. Then running:

    rustc -L path/containing/docopt/lib --pretty expanded add.rs

Will show all macros expanded. In the generated code, you should be able to 
find the generated struct:

```rust
struct Args {
    pub arg_x: int,
    pub arg_y: int,
}
```


### Traditional Docopt API

The reference implementation of Docopt returns a Python dictionary with names
like `<arg>` or `--flag`. If you prefer this access pattern, then you can use
`docopt::ArgvMap`. The disadvantage is that you have to do all of your type
conversion manually. Here's the canonical Docopt example with a hash table:

```rust
extern crate docopt;

use docopt::Docopt;

static USAGE: &'static str = "
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
    let args = Docopt::new(USAGE)
                      .and_then(|dopt| dopt.parse())
                      .unwrap_or_else(|e| e.exit());
    println!("{}", args);

    // You can conveniently access values with `get_{bool,count,str,vec}`
    // functions. If the key doesn't exist (or if, e.g., you use `get_str` on
    // a switch), then a sensible default value is returned.
    println!("\nSome values:");
    println!("  Speed: {}", args.get_str("--speed"));
    println!("  Drifting? {}", args.get_bool("--drifting"));
    println!("  Names: {}", args.get_vec("<name>"));
}
```

