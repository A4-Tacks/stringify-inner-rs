#![doc = include_str!("../README.md")]

use std::{convert::identity, str::FromStr};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing::*, Span, TokenStream, TokenTree};

fn err<T>(msg: &str, span: Span) -> Result<T, TokenStream> {
    let s = |mut t: TokenTree| {
        t.set_span(span.clone());
        t
    };
    Err(TokenStream::from_iter([
        s(Punct::new(':', Joint).into()),
        s(Punct::new(':', Joint).into()),
        s(Ident::new("core", span.clone()).into()),
        s(Punct::new(':', Joint).into()),
        s(Punct::new(':', Joint).into()),
        s(Ident::new("compile_error", span.clone()).into()),
        s(Punct::new('!', Joint).into()),
        s(Group::new(Delimiter::Parenthesis, TokenStream::from_iter([
            s(Literal::string(msg).into()),
        ])).into()),
    ]))
}

fn extract_str(s: &str, span: Span) -> Result<String, TokenStream> {
    if s.starts_with('"') { return Ok(s[1..s.len()-1].to_owned()); }
    if !s.starts_with('r') {
        return err("invalid string literal", span);
    }
    let mut s = &s[1..];
    while s.starts_with('#') {
        s = &s[1..s.len()-1];
    }
    let escaped = format!("{:?}", &s[1..s.len()-1]);
    return Ok(escaped[1..escaped.len()-1].to_string());
}

fn merge_str(a: &Literal, b: &Literal) -> Result<Literal, TokenStream> {
    let (sa, sb) = (a.span(), b.span());
    let (a, b) = (a.to_string(), b.to_string());
    let (a, b) = (extract_str(&a, sa)?, extract_str(&b, sb)?);
    Ok(Literal::from_str(&format!("\"{a}{b}\"")).unwrap())
}

fn expr_impl(stream: TokenStream) -> Result<TokenStream, TokenStream> {
    let mut result = TokenStream::new();
    let mut iter = stream.into_iter();
    while let Some(tok) = iter.next() {
        match tok {
            proc_macro::TokenTree::Group(group) => {
                let mut new_group = Group::new(
                    group.delimiter(),
                    expr_impl(group.stream())?,
                );
                new_group.set_span(group.span());
                result.extend([TokenTree::from(new_group)]);
            },
            proc_macro::TokenTree::Punct(ref punct) if punct.as_char() == '#' => {
                let Some(op) = iter.next() else {
                    return err("unexpected end of input", punct.span());
                };
                match op {
                    TokenTree::Punct(ref punct) => {
                        if punct.as_char() == '#' {
                            result.extend([op]);
                        } else {
                            return err("invalid operator", punct.span());
                        }
                    },
                    TokenTree::Group(_) => {
                        result.extend([tok]);
                        result.extend(expr_impl(
                            TokenStream::from_iter([op])
                        )?);
                    },
                    TokenTree::Literal(tt) => return err("invalid operator", tt.span()),
                    TokenTree::Ident(ident) => {
                        let Some(param) = iter.next() else {
                            return err("unexpected end of input", ident.span());
                        };
                        let TokenTree::Group(param) = param else {
                            return err("invalid operation param", param.span());
                        };
                        let out_span = param.stream().into_iter().next()
                            .map_or(param.span(), |t| t.span());
                        let param = param.stream();
                        match &*ident.to_string() {
                            "stringify" => {
                                let s = param.to_string();
                                let mut tt = Literal::string(&s);
                                tt.set_span(out_span);
                                result.extend([TokenTree::from(tt)]);
                            },
                            "concat" => {
                                let param = expr_impl(param)?;
                                let mut s = Literal::string("");
                                let mut iter = param.into_iter().peekable();
                                while let Some(tt) = iter.next() {
                                    iter.next_if(|p| matches!(p,
                                            TokenTree::Punct(p) if p.as_char() == ','));
                                    let TokenTree::Literal(lit) = tt else {
                                        return err("is not a literal", tt.span())
                                    };
                                    match merge_str(&s, &lit) {
                                        Ok(merged) => s = merged,
                                        Err(e) => return Err(e),
                                    }
                                }
                                s.set_span(out_span);
                                result.extend([TokenTree::from(s)]);
                            },
                            _ => return err("unknown operator", ident.span()),
                        }
                    },
                }
            },
            _ => result.extend([tok]),
        }
    }
    Ok(result)
}

/// Run string expressions on expressions
///
/// - `#stringify(...)`: like `stringify!(...)`
/// - `#concat(...)`: like `concat!(...)`
/// - `##`: like `#`
/// - `#[...]`: like `#[...]`
///
/// # Examples
/// ```
/// use stringify_inner::sexpr;
///
/// assert_eq!(sexpr!(#stringify(foo)), "foo");
/// assert_eq!(sexpr!(&#stringify(foo)[1..]), "oo");
/// assert_eq!(sexpr!(#concat(#stringify(foo), "bar")), "foobar");
/// ```
#[proc_macro]
pub fn sexpr(stream: TokenStream) -> TokenStream {
    expr_impl(stream).map_or_else(identity, identity)
}

/// Run string expressions on attribute
///
/// - `#stringify(...)`: like `stringify!(...)`
/// - `#concat(...)`: like `concat!(...)`
/// - `##`: like `#`
/// - `#[...]`: like `#[...]`
///
/// # Examples
/// ```
/// use stringify_inner::sexpr_attr;
///
/// #[sexpr_attr(doc(alias = #stringify(bar)))]
/// fn foo() {}
/// ```
#[proc_macro_attribute]
pub fn sexpr_attr(attr: TokenStream, item: TokenStream) -> TokenStream {
    TokenStream::from_iter([
        expr_impl(TokenStream::from_iter([
            TokenTree::from(Punct::new('#', Joint)),
            Group::new(Delimiter::Bracket, attr).into(),
        ])).unwrap_or_else(|mut e| {
            e.extend([TokenTree::from(Punct::new(';', Alone))]);
            e
        }),
        item,
    ])
}
