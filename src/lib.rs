#![doc = include_str!("../README.md")]

use core::{convert::identity, str::FromStr as _};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing::*, Span, TokenStream,
    TokenTree,
};
use proc_macro_tool::{
    rerr, stream, streams, try_pfunc, SetSpan as _, TokenTreeExt as _,
};

fn extract_str(s: &str, span: Span) -> Result<String, TokenStream> {
    if !s.ends_with(['"', '#']) {
        return rerr("invalid string suffix", span);
    }
    if s.starts_with('"') {
        return Ok(s[1..s.len()-1].to_owned());
    }
    if !s.starts_with('r') {
        return rerr("invalid string literal", span);
    }
    let mut s = &s[1..];
    while s.starts_with('#') {
        s = &s[1..s.len()-1];
    }
    let escaped = format!("{:?}", &s[1..s.len()-1]);
    debug_assert!(escaped.starts_with('"') && escaped.ends_with('"'), "{escaped}");
    Ok(escaped[1..escaped.len()-1].to_string())
}

fn merge_str(a: &Literal, b: &Literal) -> Result<Literal, TokenStream> {
    let (sa, sb) = (a.span(), b.span());
    let (a, b) = (a.to_string(), b.to_string());
    let (a, b) = (extract_str(&a, sa)?, extract_str(&b, sb)?);
    let lit = Literal::from_str(&format!("\"{a}{b}\"")).unwrap();
    Ok(lit.set_spaned(sa))
}

fn do_operation(i: Ident, group: Group) -> Result<TokenStream, TokenStream> {
    Ok(match &*i.to_string() {
        "concat" => {
            let gspan = group.span();
            let param = expr_impl(group.stream())?;
            let mut s = Literal::string("");
            let mut span = None;
            let mut iter = param.into_iter().peekable();

            while let Some(tt) = iter.next() {
                iter.next_if(|p| p.is_punch(','));
                let TokenTree::Literal(lit) = tt else {
                    return rerr("is not a literal", tt.span());
                };
                span.get_or_insert(lit.span());
                s = merge_str(&s, &lit)?;
            }

            s.set_span(span.unwrap_or(gspan));
            stream([TokenTree::from(s)])
        },
        "stringify" => {
            let out_span = group.stream().into_iter().next()
                .map_or(group.span(), |t| t.span());

            let s = group.stream().to_string();
            let tt = Literal::string(&s).set_spaned(out_span);
            stream([TokenTree::from(tt)])
        },
        _ => unreachable!(),
    })
}

fn expr_impl(stream: TokenStream) -> Result<TokenStream, TokenStream> {
    try_pfunc(
        stream,
        false,
        ["concat", "stringify"],
        do_operation,
    )
}

/// Run string expressions on expressions
///
/// - `#stringify(...)`: like `stringify!(...)`
/// - `#concat(...)`: like `concat!(...)`
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
    streams([
        expr_impl(stream([
            TokenTree::from(Punct::new('#', Joint)),
            Group::new(Delimiter::Bracket, attr).into(),
        ])).map_or_else(identity, identity),
        item,
    ])
}
