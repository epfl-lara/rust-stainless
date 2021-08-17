extern crate stainless;

pub fn main() {
  let x = (123, false);
  let mut y = x;
  let z = &mut y.0;
  *z = 456;
  assert!(y.0 == 456 && x.0 == 456)
}
