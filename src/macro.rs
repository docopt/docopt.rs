use std::collections::HashMap;
use std::gc::{Gc, GC};

use rustc::plugin::Registry;
use syntax::ast;
use syntax::codemap;
use syntax::ext::base::{ExtCtxt, MacResult, MacItem, DummyResult};
use syntax::ext::build::AstBuilder;
use syntax::parse::common::SeqSep;
use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::print::pprust;
use syntax::util::small_vector::SmallVector;

use {DEFAULT_CONFIG, Config, Docopt, ValueMap};
use parse::{
    Options,
    Atom, Short, Long, Command, Positional,
    Argument, Zero, One,
};

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("docopt", expand);
}

fn expand(cx: &mut ExtCtxt, span: codemap::Span, tts: &[ast::TokenTree])
         -> Box<MacResult> {
    let parsed = match MacParser::new(cx, tts).parse() {
        Ok(parsed) => parsed,
        Err(_) => return DummyResult::any(span),
    };
    parsed.items(cx)
}

/// Parsed corresponds to the result of parsing a `docopt` macro call.
/// It can be used to write a corresponding struct.
struct Parsed {
    struct_name: ast::Ident,
    doc: Docopt,
    /// Overrided type annotations for struct members. May be empty.
    /// When a type annotation for an atom doesn't exist, then one is
    /// inferred automatically. It is one of: `bool`, `uint`, `String` or
    /// `Vec<String>`.
    types: HashMap<Atom, Gc<ast::Ty>>,
}

impl Parsed {
    /// Returns a macro result suitable for expansion.
    /// Contains two items: one for the struct and one for the struct impls.
    fn items(&self, cx: &ExtCtxt) -> Box<MacResult> {
        let mut its = vec!();
        its.push(self.struct_decl(cx));

        let struct_name = self.struct_name;
        let full_doc = self.doc.p.full_doc.as_slice();
        its.push(quote_item!(cx,
            impl docopt::FlagParser for $struct_name {
                #[allow(dead_code)]
                fn parse_args(conf: docopt::Config, args: &[&str])
                             -> $struct_name {
                    docopt::docopt_args(conf, args, $full_doc).decode_must()
                }
            }
        ).unwrap());
        MacItems::new(its)
    }

    /// Returns an item for the struct definition.
    fn struct_decl(&self, cx: &ExtCtxt) -> Gc<ast::Item> {
        let def = ast::StructDef {
            fields: self.struct_fields(cx),
            ctor_id: None,
            super_struct: None,
            is_virtual: false,
        };
        let sp = codemap::DUMMY_SP;
        let traits = vec![meta_item(cx, "Decodable"), meta_item(cx, "Show")];
        let deriving = cx.meta_list(sp, intern("deriving"), traits);
        let attrs = vec![cx.attribute(codemap::DUMMY_SP, deriving)];
        let st = cx.item_struct(sp, self.struct_name.clone(), def);
        cx.item(sp, self.struct_name.clone(), attrs, st.node.clone())
    }

    /// Returns a list of fields for the struct definition.
    /// Handles type annotations.
    fn struct_fields(&self, cx: &ExtCtxt) -> Vec<ast::StructField> {
        let mut fields: Vec<ast::StructField> = vec!();
        for (atom, opts) in self.doc.p.descs.iter() {
            let name = ValueMap::key_to_struct_field(atom.to_str().as_slice());
            let ty = match self.types.find(atom) {
                None => self.pat_type(cx, atom, opts),
                Some(ty) => ty.clone(),
            };
            fields.push(self.mk_struct_field(name.as_slice(), ty));
        }
        fields
    }

    /// Returns an inferred type for a usage pattern.
    /// This is only invoked when a type annotation is not present.
    fn pat_type(&self, cx: &ExtCtxt, atom: &Atom, opts: &Options) -> Gc<ast::Ty> {
        match (opts.repeats, &opts.arg) {
            (false, &Zero) => {
                match atom {
                    &Positional(_) => quote_ty!(cx, String),
                    _ => quote_ty!(cx, bool),
                }
            }
            (true, &Zero) => {
                match atom {
                    &Positional(_) => quote_ty!(cx, Vec<String>),
                    _ => quote_ty!(cx, uint),
                }
            }
            (false, &One(_)) => quote_ty!(cx, String),
            (true, &One(_)) => quote_ty!(cx, Vec<String>),
        }
    }

    /// Creates a struct field from a member name and type.
    fn mk_struct_field(&self, name: &str, ty: Gc<ast::Ty>) -> ast::StructField {
        codemap::dummy_spanned(ast::StructField_ {
            kind: ast::NamedField(ident(name), ast::Public),
            id: ast::DUMMY_NODE_ID,
            ty: ty,
            attrs: vec!(),
        })
    }
}

/// State for parsing a `docopt` macro invocation.
struct MacParser<'a, 'b> {
    cx: &'b mut ExtCtxt<'a>,
    p: Parser<'a>,
}

impl<'a, 'b> MacParser<'a, 'b> {
    fn new(cx: &'b mut ExtCtxt<'a>, tts: &[ast::TokenTree]) -> MacParser<'a, 'b> {
        let p = cx.new_parser_from_tts(tts);
        MacParser { cx: cx, p: p }
    }

    /// Main entry point for parsing arguments to `docopt` macro.
    /// First looks for an identifier for the struct name.
    /// Second, a string containing the docopt usage patterns.
    /// Third, an optional list of type annotations.
    fn parse(&mut self) -> Result<Parsed, ()> {
        if self.p.token == token::EOF {
            self.cx.span_err(self.cx.call_site(), "macro expects arguments");
            return Err(());
        }
        let struct_name = self.p.parse_ident();
        self.p.expect(&token::COMMA);
        let docstr = try!(self.parse_str());

        let sep = SeqSep {
            sep: Some(token::COMMA),
            trailing_sep_allowed: true,
        };
        let types = self.p.parse_seq_to_end(
            &token::EOF, sep, |p| MacParser::parse_type_annotation(p)
        ).move_iter()
         .map(|(ident, ty)| {
             let field_name = token::get_ident(ident).to_str();
             let key = ValueMap::struct_field_to_key(field_name.as_slice());
             (Atom::new(key.as_slice()), ty)
          })
         .collect::<HashMap<Atom, Gc<ast::Ty>>>();
        self.p.expect(&token::EOF);


        // This config does not matter because we're only asking for the
        // usage patterns in the Docopt string. The configuration does not
        // affect the retrieval of usage patterns.
        let doc = match Docopt::new(DEFAULT_CONFIG.clone(), docstr.as_slice()) {
            Ok(doc) => doc,
            Err(err) => {
                self.cx.span_err(self.cx.call_site(),
                                 format!("Invalid Docopt usage: {}",
                                         err).as_slice());
                return Err(());
            }
        };
        Ok(Parsed {
            struct_name: struct_name,
            doc: doc,
            types: types,
        })
    }

    /// Parses a single string literal. On failure, an error is logged and
    /// unit is returned.
    fn parse_str(&mut self) -> Result<String, ()> {
        fn lit_is_str(lit: Gc<ast::Lit>) -> bool {
            match lit.node {
                ast::LitStr(_, _) => true,
                _ => false,
            }
        }
        fn lit_to_str(lit: Gc<ast::Lit>) -> String {
            match lit.node {
                ast::LitStr(ref s, _) => s.to_str(),
                _ => fail!("BUG: expected string literal"),
            }
        }
        let exp = self.cx.expand_expr(self.p.parse_expr());
        let s = match exp.node {
            ast::ExprLit(lit) if lit_is_str(lit) => lit_to_str(lit),
            _ => {
                let err = format!("Expected string literal but got {}",
                                  pprust::expr_to_str(exp));
                self.cx.span_err(exp.span, err.as_slice());
                return Err(());
            }
        };
        self.p.bump();
        Ok(s)
    }

    /// Parses a type annotation in a `docopt` invocation of the form
    /// `ident: Ty`.
    /// Note that this is a static method as it is used as a HOF.
    fn parse_type_annotation(p: &mut Parser) -> (ast::Ident, Gc<ast::Ty>) {
        let ident = p.parse_ident();
        p.expect(&token::COLON);
        let ty = p.parse_ty(false);
        (ident, ty)
    }
}

/// MacItems is just like `syntax::ext::base::MacItem`, except it supports
/// writing multiple items.
struct MacItems {
    its: Vec<Gc<ast::Item>>,
}

impl MacResult for MacItems {
    fn make_items(&self) -> Option<SmallVector<Gc<ast::Item>>> {
        let mut small = SmallVector::zero();
        for it in self.its.iter() {
            small.push(it.clone());
        }
        Some(small)
    }
}

impl MacItems {
    fn new(its: Vec<Gc<ast::Item>>) -> Box<MacResult> {
        box MacItems { its: its } as Box<MacResult>
    }
}

// Convenience functions for building intermediate values.

fn ident(s: &str) -> ast::Ident {
    ast::Ident::new(token::intern(s))
}

fn meta_item(cx: &ExtCtxt, s: &str) -> Gc<ast::MetaItem> {
    cx.meta_word(codemap::DUMMY_SP, intern(s))
}

fn intern(s: &str) -> token::InternedString {
    token::intern_and_get_ident(s)
}
