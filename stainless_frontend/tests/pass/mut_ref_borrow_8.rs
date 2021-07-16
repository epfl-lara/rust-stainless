extern crate stainless;

struct S<T>(T);
struct Container<T> {
  s: S<T>,
}

pub fn main() {
  let mut container = Container { s: S("hello") };

  let inner = &mut container.s;
  *inner = S("world");

  let field = &mut inner.0;
  *field = "!";

  assert!(matches!(container.s, S("!")))
}
