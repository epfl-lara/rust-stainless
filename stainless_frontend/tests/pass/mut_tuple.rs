extern crate stainless;

pub fn main() {
  let mut x = (123, false);
  let y = x;
  x.1 = true;
  assert!(!y.1 && x.1)
}
