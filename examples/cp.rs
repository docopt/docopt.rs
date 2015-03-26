extern crate rustc_serialize;
extern crate docopt;

use docopt::Docopt;

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: cp [-a] <source> <dest>
       cp [-a] <source>... <dir>

Options:
    -a, --archive  Copy everything.
";

#[derive(Debug, RustcDecodable)]
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
    println!("{:?}", args);
}
