extern crate stainless;

pub struct S(i32, i32);

pub fn main() {
  let mut a = S(1, 2);

  let b = &mut a;
  *b = S(100, 200);

  assert!(a.0 == 100 && a.1 == 200)
}
