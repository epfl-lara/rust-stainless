extern crate stainless;

struct A {
  b: bool,
}

fn set_false(mut a: A) -> A {
  a.b = false;
  a
}
