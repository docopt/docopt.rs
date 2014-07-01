use {Value, Switch, Counted, Plain, List};
use test::{get_args, map_from_alist, same_args};

test_expect!(test_1, "
Usage: prog
", &[], vec!())

test_user_error!(test_2, "
Usage: prog
", &["--xxx"])

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
