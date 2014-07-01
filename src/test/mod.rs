use std::collections::HashMap;
use {Docopt, Config, ValueMap};
use {Value, Switch, Counted, Plain, List};

static conf: Config = Config {
    options_first: false,
    help: true,
    version: None,
};

fn get_args(doc: &str, argv: &[&'static str]) -> ValueMap {
    let dopt =
        match Docopt::new(doc, conf.clone()) {
            Err(err) => fail!("Invalid usage: {}", err),
            Ok(dopt) => dopt,
        };
    match dopt.argv(argv) {
        Err(err) => fail!("{}", err),
        Ok(vals) => vals,
    }
}

fn map_from_alist(alist: Vec<(&'static str, Value)>) -> HashMap<String, Value> {
    alist.move_iter().map(|(k, v)| (k.to_string(), v)).collect()
}

fn same_args(expected: &HashMap<String, Value>, got: &ValueMap) {
    // debug!("GOT: {}", got); 
    // debug!("EXPECTED: {}", expected); 
    // debug!("---------------"); 
    for (k, v) in expected.iter() {
        assert!(v == got.map.get(k), "EXPECTED KEY: {}", k);
    }
    for (k, v) in got.map.iter() {
        assert!(v == expected.get(k), "GOT KEY: {}", k);
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
)

macro_rules! test_user_error(
    ($name:ident, $doc:expr, $args:expr) => (
        #[test]
        #[should_fail]
        fn $name() { get_args($doc, $args); }
    );
)

mod testcases;
