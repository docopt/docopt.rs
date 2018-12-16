use {Docopt, Error};

fn get_suggestion(doc: &str, argv: &[&'static str]) -> Error {
    let dopt =
        match Docopt::new(doc) {
            Err(err) => panic!("Invalid usage: {}", err),
            Ok(dopt) => dopt,
        };
    let mut argv: Vec<_> = argv.iter().map(|x| x.to_string()).collect();
    argv.insert(0, "prog".to_string());
    match dopt.argv(argv.into_iter()).parse() {
        Err(err) => err,
        Ok(_) => panic!("Should have been a user error"),
    }
}

macro_rules! test_suggest(
    ($name:ident, $doc:expr, $args:expr, $expected:expr) => (
        #[test]
        fn $name() {
            let sg = get_suggestion($doc, $args);
            println!("{}", sg);
            match sg {
                Error::WithProgramUsage(e, _) => {
                    match *e {
                        Error::Argv(msg) => {
                            println!("{:?}",msg);
                            assert_eq!(msg, $expected);
                        }
                        err => panic!("Error other than argv: {:?}", err)
                    }
                }, 
                _ => panic!("Error without program usage")
            }
        }
    );
);


test_suggest!(test_suggest_1, "Usage: prog [--release]", &["--releas"], "Unknown flag: '--releas'. Did you mean '--release'?"); 

test_suggest!(test_suggest_2, 
"Usage: prog [-a] <source> <dest>
        prog [-a] <source>... <dir>
        prog [-e]
 Options:
    -a, --archive  Copy everything.
", 
&["-d"], "Unknown flag: '-d'"); 


test_suggest!(test_suggest_3, 
"Usage: prog [-a] <source> <dest>
        prog [-a] <source>... <dir>
        prog [-e]
 Options:
    -a, --archive  Copy everything.
    -e, --export Export all the things.
", 
&["--expotr"], "Unknown flag: '--expotr'. Did you mean '--export'?"); 


test_suggest!(test_suggest_4, 
"Usage: prog [--import] [--complete]
", 
&["--mport", "--complte"], "Unknown flag: '--mport'. Did you mean '--import'?"); 

test_suggest!(test_suggest_5, 
"Usage: prog [--import] [--complete]
", 
&["--import", "--complte"], "Unknown flag: '--complte'. Did you mean '--complete'?"); 

test_suggest!(test_suggest_6,
"Usage: cargo owner [options]

 Options:
    -h, --help               Print this message
    -a, --add LOGIN          Name of a user or team to add as an owner
    -r, --remove LOGIN       Name of a user or team to remove as an owner
    -l, --list               List owners of a crate
",
&["owner", "add"], "Unknown command: 'add'. Did you mean '--add'?");

test_suggest!(test_suggest_7,
"Usage: cargo owner [options]

 Options:
    -h, --help               Print this message
    -a, --add LOGIN          Name of a user or team to add as an owner
    -r, --remove LOGIN       Name of a user or team to remove as an owner
    -l, --list               List owners of a crate
",
&["owner", "list"], "Unknown command: 'list'. Did you mean '--list'?");
