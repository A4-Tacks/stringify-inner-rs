This macro translates a simple string processing expression,
such as `#stringify()` and `#concat()`

Can be used in conjunction with some macros that cannot evaluate `stringify!()`

# Examples
Is compile-fail case:
```rust,compile_fail
#[doc(alias = stringify!(bar))]
fn foo() {}
```

Change to:

```rust
use stringify_inner::sexpr_attr;

#[sexpr_attr(doc(alias = #stringify(bar)))]
fn foo() {}
```

```rust
use stringify_inner::sexpr;

assert_eq!(sexpr!(#stringify(foo)), "foo");
assert_eq!(sexpr!(#concat(#stringify(foo), "bar")), "foobar");
```
