extern crate stainless;

pub fn main() {
  let mut x = 1;
  assert!(x == 1);

  // This should fail because we disallow mutable borrow.
  let y = &mut x;
  assert!(*y == 1);
}
