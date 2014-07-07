Docopt for Rust with automatic type based decoding (i.e., data validation).
This implementation conforms to the 
[official description of Docopt](http://docopt.org/) and
[passes its test suite](https://github.com/docopt/docopt/pull/201).

[![Build status](https://api.travis-ci.org/BurntSushi/docopt.rs.png)](https://travis-ci.org/BurntSushi/docopt.rs)

Licensed under the [UNLICENSE](http://unlicense.org).


### Current status
Fully functional but the design of the API is up for debate. **I am seeking 
feedback**.


### Documentation

[http://burntsushi.net/rustdoc/docopt](http://burntsushi.net/rustdoc/docopt/index.html)

There are several examples and most things are documented, but not quite well 
enough yet.


### Quick example

Here is a full working example:

```rust
#![feature(phase)]
extern crate serialize;
#[phase(plugin, link)] extern crate docopt;

docopt!(Args, "
Usage: cp [-a] SOURCE DEST
       cp [-a] SOURCE... DIR

Options:
    -a, --archive  Copy everything.
")

fn main() {
    let args = Args::parse();

    // The Args struct satisfies `Show`:
    println!("{}", args);

    // Try running with `example -a file1 file2 dest/`.
    assert!(args.flag_archive);
    assert_eq!(args.arg_SOURCE, vec!["file1".to_string(), "file2".to_string()]);
    assert_eq!(args.arg_DIR, "dest/".to_string());
}
```

The `docopt!` macro will create a struct for you! The field names map like 
this:

```
-g       => flag_g
--group  => flag_group
FILE     => arg_FILE
<file>   => arg_file
build    => cmd_build
```

The `Args` struct has three static methods defined for it: `parse`, 
`parse_conf` and `parse_args`. These correspond to the module functions
[docopt](http://burntsushi.net/rustdoc/docopt/fn.docopt.html),
[docopt_conf](http://burntsushi.net/rustdoc/docopt/fn.docopt_conf.html)
and [docopt_args](http://burntsushi.net/rustdoc/docopt/fn.docopt_args.html)
respectively. (The only difference is that the `parse_*` methods don't
take a Docopt string.)


### Data validation example

Here's another example that shows how to specify the types of your arguments:

```rust
#![feature(phase)]
extern crate serialize;
#[phase(plugin, link)] extern crate docopt;

docopt!(Args, "Usage: add <x> <y>", arg_x: int, arg_y: int)

fn main() {
    let args = Args::parse();
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


### Modeling `rustc`

Here's a selected subset for some of `rustc`'s options. This also shows how to 
restrict values to a list of choices via an `enum` type.

```rust
#![feature(phase)]
extern crate serialize;
#[phase(plugin, link)] extern crate docopt;

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
            // This is a wart. How can it be fixed?
            _ => fail!("How to CONVENIENTLY create value with type `E`?"),
        })
    }
}

fn main() {
    let args = Args::parse();
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

### Macros, decoding and hash tables

**I need help designing the API**. It seems as though there are roughly three
levels increasing magic at which one can use this library:

1. Parse command line arguments into a hash table. The values of this table
   are [Value](http://burntsushi.net/rustdoc/docopt/type.Value.html) enums.
   This roughly corresponds to the API provided by the reference Python
   implementation of Docopt. e.g., keys are `--flag` or `-f` or `cmd` or
   `<arg>` or `ARG`. The downside of this approach is that you must deal with
   `Value` everywhere.
2. Parse command line arguments into a hash table and then decode these values
   into a struct. The magic here is the conversion from `Value` types to your
   own types, which must satisfy the `Decodable` trait. This is useful when
   your values should be integers or floats or enumerations because Docopt
   proper only knows about the following four types: booleans, counts, strings
   and lists of strings. The downside of this approach is that you have to
   define a struct independent of your Docopt usage string, which violates one
   of its most important features: single point of truth.
3. Use a macro that *creates a struct for you* from the Docopt usage string.
   Decoding into that struct would work as in (2). The problem with this
   approach is that it is very magical given that you don't actually get to
   see the definition of your struct.

