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

#[test]
fn test_1() {
    let doc = "
Usage: prog
";
    let args = &[];
    let vals = get_args(doc, args);
    
    let expected = map_from_alist(vec!());
    same_args(&expected, &vals);
}

#[test]
#[should_fail]
fn test_2() {
    let doc = "
Usage: prog
";
    let args = &["--xxx"];
    let vals = get_args(doc, args);
}

#[test]
fn test_3() {
    let doc = "
Usage: prog [options]

Options:
  -a  All.
";
    let args = &[];
    let vals = get_args(doc, args);
    let expected = map_from_alist(vec!(("-a", Switch(false))));
    same_args(&expected, &vals);
}

#[test]
fn test_4() {
    let doc = "
Usage: prog [options]

Options:
  -a  All.
";
    let args = &["-a"];
    let vals = get_args(doc, args);
    let expected = map_from_alist(vec!(("-a", Switch(true))));
    same_args(&expected, &vals);
}

#[test]
#[should_fail]
fn test_5() {
    let doc = "
Usage: prog [options]

Options:
  -a  All.
";
    let args = &["-x"];
    let vals = get_args(doc, args);
}
