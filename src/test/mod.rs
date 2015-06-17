use std::collections::HashMap;
use {Docopt, ArgvMap};
use Value::{self, Switch, Plain};

fn get_args(doc: &str, argv: &[&'static str]) -> ArgvMap {
    let dopt = match Docopt::new(doc) {
        Err(err) => panic!("Invalid usage: {}", err),
        Ok(dopt) => dopt,
    };
    match dopt.argv(vec!["cmd"].iter().chain(argv.iter()).cloned()).parse() {
        Err(err) => panic!("{}", err),
        Ok(vals) => vals,
    }
}

fn map_from_alist(alist: Vec<(&'static str, Value)>) -> HashMap<String, Value> {
    alist.into_iter().map(|(k, v)| (k.to_string(), v)).collect()
}

fn same_args(expected: &HashMap<String, Value>, got: &ArgvMap) {
    for (k, ve) in expected.iter() {
        match got.map.find(k) {
            None => panic!("EXPECTED has '{}' but GOT does not.", k),
            Some(vg) => assert!(ve == vg,
                                "{}: EXPECTED = '{:?}' != '{:?}' = GOT", k, ve, vg),
        }
    }
    for (k, vg) in got.map.iter() {
        match got.map.find(k) {
            None => panic!("GOT has '{}' but EXPECTED does not.", k),
            Some(ve) => assert!(vg == ve,
                                "{}: GOT = '{:?}' != '{:?}' = EXPECTED", k, vg, ve),
        }
    }
}

macro_rules! test_expect(
    ($name:ident, $doc:expr, $args:expr, $expected:expr) => (
        #[test]
        fn $name() {
            let vals = get_args($doc, $args);
            let expected = map_from_alist($expected);
            same_args(&expected, &vals);
        }
    );
);

macro_rules! test_user_error(
    ($name:ident, $doc:expr, $args:expr) => (
        #[test]
        #[should_panic]
        fn $name() { get_args($doc, $args); }
    );
);

test_expect!(test_issue_13, "Usage: prog file <file>", &["file", "file"],
             vec![("file", Switch(true)),
                  ("<file>", Plain(Some("file".to_string())))]);

test_expect!(test_issue_129, "Usage: prog [options]

Options:
    --foo ARG   Foo foo.",
             &["--foo=a b"],
             vec![("--foo", Plain(Some("a b".into())))]);

mod testcases;
mod suggestions;
