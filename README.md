Docopt for Rust with automatic type based decoding (i.e., data validation).

[![Build status](https://api.travis-ci.org/BurntSushi/docopt.rs.png)](https://travis-ci.org/BurntSushi/docopt.rs)

Licensed under the [UNLICENSE](http://unlicense.org).

Check out
[examples/no_macro.rs](https://github.com/BurntSushi/docopt.rs/blob/master/examples/no_macro.rs)
for a glimpse at Docopt in action. (As the name of the example suggests, there
is no macro support yet.)

### TODO

* Testing.
* Macro.


### Macro ideas

The main focus of Docopt is that the usage descriptions represent a *single
point of truth* about the available command line options. In a dynamic language
like Python, this is trivially and *idiomatically* represented as a dictionary.
It is sensible to do the same in Rust, but with a hash map. However, things can 
get a bit cumbersome because Docopt only knows about the following types: 
boolean, count, string and list of strings. So in most usages, you'd probably
have to define your own "config" struct with the right types, and then convert 
the hash map given by Docopt into your config struct.

This is not so bad because it can be done by implementing the 
`serialize::Decoder` trait. The problem with this approach is that it violates 
the single point of truth property: you end up declaring your config with a 
Docopt string and then you have to do it again with your "config" struct, but 
with more type specific annotations.

This is where a procedural macro can help: it will write your struct definition 
for you based on the Docopt string. The macro would accept type annotations for 
options:

```rust
docopt!(StructName, "docopt usage string",
        -n: uint, FILE: Path, <name>: MyNameType)
```

The types would need to satisfy some `FromDocoptValue` trait. If a type isn't 
specified for an option, then it will fallback to the natural type as indicated 
by Docopt (e.g., boolean -> `bool`, count -> `uint`, string -> `String` and
list of strings -> `Vec<String>`).

The macro solves our single point of truth problem *and* it makes it convenient 
to read command line options because you don't need to manually convert values 
to the types you want to use to represent them.

The fundamental problem with this approach is that it is **magical**. It feels 
very wrong for a struct to be generated for you.

