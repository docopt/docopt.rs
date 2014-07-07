use std::collections::HashMap;
use {Docopt, Config, ValueMap, DEFAULT_CONFIG};
use {Value, Switch, Counted, Plain, List};

fn get_args(doc: &str, argv: &[&'static str]) -> ValueMap {
    let dopt =
        match Docopt::new(DEFAULT_CONFIG.clone(), doc) {
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
    for (k, ve) in expected.iter() {
        match got.map.find(k) {
            None => fail!("EXPECTED has '{}' but GOT does not.", k),
            Some(vg) => assert!(ve == vg,
                                "{}: EXPECTED = '{}' != '{}' = GOT", k, ve, vg),
        }
    }
    for (k, vg) in got.map.iter() {
        match got.map.find(k) {
            None => fail!("GOT has '{}' but EXPECTED does not.", k),
            Some(ve) => assert!(vg == ve,
                                "{}: GOT = '{}' != '{}' = EXPECTED", k, vg, ve),
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
)

macro_rules! test_user_error(
    ($name:ident, $doc:expr, $args:expr) => (
        #[test]
        #[should_fail]
        fn $name() { get_args($doc, $args); }
    );
)

mod testcases;
