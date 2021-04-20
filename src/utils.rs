//! Utilities that needed a home.


/// Wrapper for lazily compiled regexes
pub struct RegexWrap(&'static str, ::once_cell::sync::OnceCell<::regex::Regex>);

impl RegexWrap {
    /// Create a new const instances with the given regexp
    pub const fn new(re: &'static str) -> Self {
        Self(
            re,
            ::once_cell::sync::OnceCell::<::regex::Regex>::new()
        )
    }
}

impl ::std::ops::Deref for RegexWrap {
    type Target = ::regex::Regex;
    fn deref(&self) -> &Self::Target {
        self.1.get_or_init(|| ::regex::Regex::new(self.0).unwrap())
    }
}


/// Declares a lazy static regex
macro_rules! decl_regex {
    ($($name:ident : $re:literal; )*) => {
        $(
            static $name: $crate::utils::RegexWrap = $crate::utils::RegexWrap::new($re);
        )*
    };
}

/// Print an error.
macro_rules! werr(
    ($($arg:tt)*) => ({
        use std::io::{Write, stderr};
        write!(&mut stderr(), $($arg)*).unwrap();
    })
);


pub(crate) fn cap_or_empty<'t>(caps: &regex::Captures<'t>, name: &str) -> &'t str {
    caps.name(name).map_or("", |m| m.as_str())
}
