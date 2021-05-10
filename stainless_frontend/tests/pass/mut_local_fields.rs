#![allow(dead_code, unused_assignments)]

extern crate stainless;

struct S {
  field: i32,
}

fn set_field(mut s: S) -> S {
  s.field = 456;
  s
}

struct Outer(S);

fn set_inner_field(mut o: Outer) -> Outer {
  o.0.field = 101112;
  o
}

struct Gen<T>(T);

fn set_gen_field(mut g: Gen<S>) -> Gen<S> {
  g.0.field = 131415;
  g = Gen(S { field: 161718 });
  g
}

// As control, the same for a primitive type
fn set_int(mut s: i32) -> i32 {
  s = 1000;
  s
}

pub fn main() {
  // field assignment
  let mut s = S { field: 123 };
  assert!(s.field == 123);
  s = set_field(s);
  assert!(s.field == 456);

  s.field = -111;
  assert!(s.field == -111);

  let o = Outer(s);
  let o = set_inner_field(o);
  assert!(o.0.field == 101112);

  let s = S { field: 123 };
  let g = Gen(s);
  let g = set_gen_field(g);
  assert!(g.0.field == 161718);

  let i = set_int(12);
  assert!(i == 1000);
}
