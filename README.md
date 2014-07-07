Docopt for Rust with automatic type based decoding (i.e., data validation).
This implementation conforms to the 
[official description of Docopt](http://docopt.org/) and
[passes its test suite](https://github.com/docopt/docopt/pull/201).

[![Build status](https://api.travis-ci.org/BurntSushi/docopt.rs.png)](https://travis-ci.org/BurntSushi/docopt.rs)

Licensed under the [UNLICENSE](http://unlicense.org).


### Current status
Fully functional but the design of the API is up for debate. **I am seeking 
feedback**.


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
    // Try running with `example -a file1 file2 dest/`.
    let args = Args::parse();
    assert!(args.flag_archive);
    assert_eq!(args.arg_SOURCE, vec!["file1".to_string(), "file2".to_string()]);
    assert_eq!(args.arg_DIR, "dest/".to_string());
}
```


### Documentation

[http://burntsushi.net/rustdoc/docopt](http://burntsushi.net/rustdoc/docopt/index.html)

There are several examples and most things are documented, but not quite well 
enough yet.


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

