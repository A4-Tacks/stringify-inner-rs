#![doc = include_str!("../README.md")]

use core::{convert::identity, str::FromStr as _};

use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing::*, Span, TokenStream,
    TokenTree,
};

#[must_use]
fn stream<I>(iter: I) -> TokenStream
where I: IntoIterator,
      TokenStream: FromIterator<I::Item>,
{
    TokenStream::from_iter(iter)
}

fn err<T>(msg: &str, span: Span) -> Result<T, TokenStream> {
    let s = |mut t: TokenTree| {
        t.set_span(span);
        t
    };
    Err(stream([
        s(Punct::new(':', Joint).into()),
        s(Punct::new(':', Joint).into()),
        s(Ident::new("core", span).into()),
        s(Punct::new(':', Joint).into()),
        s(Punct::new(':', Joint).into()),
        s(Ident::new("compile_error", span).into()),
        s(Punct::new('!', Joint).into()),
        s(Group::new(Delimiter::Brace, stream([
            s(Literal::string(msg).into()),
        ])).into()),
    ]))
}

fn extract_str(s: &str, span: Span) -> Result<String, TokenStream> {
    if !s.ends_with(['"', '#']) {
        return err("invalid string suffix", span);
    }
    if s.starts_with('"') {
        return Ok(s[1..s.len()-1].to_owned());
    }
    if !s.starts_with('r') {
        return err("invalid string literal", span);
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
    let mut lit = Literal::from_str(&format!("\"{a}{b}\"")).unwrap();
    lit.set_span(sa);
    Ok(lit)
}

fn do_operation(
    tok: TokenTree,
    iter: &mut impl Iterator<Item = TokenTree>,
) -> Result<TokenStream, TokenStream> {
    let Some(op) = iter.next() else {
        return Ok(stream([tok]));
    };
    let degrade = || stream([tok.clone(), op.clone()]);
    Ok(match op {
        TokenTree::Punct(ref punct)
            if punct.as_char() == '#' =>
        {
            stream([op])
        },
        TokenTree::Punct(_) => {
            degrade()
        },
        TokenTree::Group(_) => {
            stream([
                stream([tok]),
                expr_impl(stream([op]))?,
            ])
        },
        TokenTree::Literal(_) => degrade(),
        TokenTree::Ident(ref ident) => {
            let Some(param) = iter.next() else {
                return Ok(degrade());
            };
            let TokenTree::Group(param) = param else {
                return Ok(degrade());
            };
            let gspan = param.span();

            match &*ident.to_string() {
                "stringify" => {
                    let out_span = param.stream().into_iter().next()
                        .map_or(param.span(), |t| t.span());

                    let s = param.stream().to_string();
                    let mut tt = Literal::string(&s);
                    tt.set_span(out_span);
                    stream([TokenTree::from(tt)])
                },
                "concat" => {
                    let param = expr_impl(param.stream())?;
                    let mut s = Literal::string("");
                    let mut span = None;
                    let mut iter = param.into_iter().peekable();

                    while let Some(tt) = iter.next() {
                        iter.next_if(|p| matches!(p,
                                TokenTree::Punct(p) if p.as_char() == ','));
                        let TokenTree::Literal(lit) = tt else {
                            return err("is not a literal", tt.span());
                        };
                        span.get_or_insert(lit.span());
                        s = merge_str(&s, &lit)?;
                    }

                    s.set_span(span.unwrap_or(gspan));
                    stream([TokenTree::from(s)])
                },
                _ => return Ok(degrade()),
            }
        },
    })
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
            proc_macro::TokenTree::Punct(ref punct)
                if punct.as_char() == '#' =>
            {
                result.extend(do_operation(tok, &mut iter)?);
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
    stream([
        expr_impl(stream([
            TokenTree::from(Punct::new('#', Joint)),
            Group::new(Delimiter::Bracket, attr).into(),
        ])).map_or_else(identity, identity),
        item,
    ])
}
