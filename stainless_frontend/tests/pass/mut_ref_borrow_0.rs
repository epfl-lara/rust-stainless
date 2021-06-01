extern crate stainless;

pub fn main() {
  let mut x = 1;

  let y = &mut x;
  assert!(*y == 1);
  *y = 2;

  assert!(x == 2)
}
