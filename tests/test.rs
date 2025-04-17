use stringify_inner::{sexpr, sexpr_attr};

#[test]
fn test_expr() {
    assert_eq!(sexpr!(#stringify(foo)), "foo");
    assert_eq!(sexpr!(&#stringify(foo)[1..]), "oo");
    assert_eq!(sexpr!((([&#stringify(foo)[1..]]))), ["oo"]);
    assert_eq!(sexpr!(#concat(#stringify(foo) "bar")), "foobar");
    assert_eq!(sexpr!(#concat(#stringify(foo), "bar")), "foobar");
    assert_eq!(sexpr!(#concat(#stringify(foo), "\nbar")), "foo\nbar");
    assert_eq!(sexpr!(#concat(#stringify(foo), r"bar")), "foobar");
    assert_eq!(sexpr!(#concat(#stringify(foo), r#"bar"#)), "foobar");
    assert_eq!(sexpr!(#concat(#stringify(foo), r##"bar"##)), "foobar");
    assert_eq!(sexpr!(#concat(#stringify(foo), r##"\nbar"##)), "foo\\nbar");
    assert_eq!(sexpr!(#concat(#stringify(#stringify(foo)) "bar")), "#stringify(foo)bar");
    assert_eq!(sexpr!(#concat("foo\n", "\nbar")), "foo\n\nbar");
    assert_eq!(sexpr!(#concat(r"foo\n", "\nbar")), "foo\\n\nbar");
    assert_eq!(sexpr!(#concat(r#"foo\n"#, "\nbar")), "foo\\n\nbar");
    assert_eq!(sexpr!(#concat(r"foo\n", "\nbar")), "foo\\n\nbar");
    assert_eq!(sexpr!(#concat(r"foo\n", "\nbar", "\nbaz")), "foo\\n\nbar\nbaz");
    assert_eq!(sexpr!(#concat()), "");
    assert_eq!(sexpr!(#concat("a", "b")), "ab");
    assert_eq!(sexpr!(#concat("a", ".\"b")), "a.\"b");
    assert_eq!(sexpr!(#stringify()), "");
    assert_eq!(sexpr!(#stringify(+ +)), "+ +");
    assert_eq!(sexpr!(#stringify(+  +)), "+ +");
    assert_eq!(sexpr!(#stringify(++)), "++");
    assert_eq!(sexpr!(#stringify(++)), stringify!(++));
    assert_eq!(sexpr!(#stringify(+ +)), stringify!(+ +));
    assert_eq!(sexpr!(#stringify(=>)), stringify!(=>));
    assert_eq!(sexpr!(#stringify(1+2)), stringify!(1+2));
    assert_eq!(sexpr!(#stringify(1+ 2)), stringify!(1+ 2));
}

#[sexpr_attr(doc(alias = #stringify(foo)))]
struct _Test;
