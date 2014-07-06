use std::gc::{Gc, GC};

use rustc::plugin::Registry;
use syntax::ast;
use syntax::codemap;
use syntax::ext::base::{ExtCtxt, MacResult, MacItem, DummyResult};
use syntax::ext::build::AstBuilder;
use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::print::pprust;
use syntax::util::small_vector::SmallVector;

use {Config, Docopt, ValueMap};
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

struct Parsed {
    struct_name: ast::Ident,
    doc: Docopt,
}

impl Parsed {
    fn items(&self, cx: &ExtCtxt) -> Box<MacResult> {
        let struct_name = self.struct_name;
        let mut its = vec!();
        its.push(self.struct_decl(cx));
        its.push(quote_item!(cx,
            impl $struct_name {
                fn wat(&self) {
                    println!("hiya!");
                }
            }
        ).unwrap());
        MacItems::new(its)
    }

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

    fn struct_fields(&self, cx: &ExtCtxt) -> Vec<ast::StructField> {
        let mut fields: Vec<ast::StructField> = vec!();
        for (atom, opts) in self.doc.p.descs.iter() {
            let name = ValueMap::key_to_struct_field(atom.to_str().as_slice());
            let ty = self.pat_type(cx, atom, opts);
            fields.push(self.mk_struct_field(name.as_slice(), ty));
        }
        fields
    }

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

    fn mk_struct_field(&self, name: &str, ty: Gc<ast::Ty>) -> ast::StructField {
        codemap::dummy_spanned(ast::StructField_ {
            kind: ast::NamedField(ident(name), ast::Public),
            id: ast::DUMMY_NODE_ID,
            ty: ty,
            attrs: vec!(),
        })
    }
}

fn ident(s: &str) -> ast::Ident {
    ast::Ident::new(token::intern(s))
}

fn meta_item(cx: &ExtCtxt, s: &str) -> Gc<ast::MetaItem> {
    cx.meta_word(codemap::DUMMY_SP, intern(s))
}

fn intern(s: &str) -> token::InternedString {
    token::intern_and_get_ident(s)
}

struct MacParser<'a, 'b> {
    cx: &'b mut ExtCtxt<'a>,
    p: Parser<'a>,
}

impl<'a, 'b> MacParser<'a, 'b> {
    fn new(cx: &'b mut ExtCtxt<'a>, tts: &[ast::TokenTree]) -> MacParser<'a, 'b> {
        let p = cx.new_parser_from_tts(tts);
        MacParser { cx: cx, p: p }
    }

    fn parse(&mut self) -> Result<Parsed, ()> {
        if self.p.token == token::EOF {
            self.cx.span_err(self.cx.call_site(), "macro expects arguments");
            return Err(());
        }
        let struct_name = self.p.parse_ident();
        self.p.expect(&token::COMMA);
        let docstr = try!(self.parse_str());

        let conf = Config { options_first: false, help: true, version: None };
        let doc = match Docopt::new(docstr.as_slice(), conf) {
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
        })
    }

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
}

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
