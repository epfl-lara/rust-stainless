extern crate stainless;

#[derive(Clone)]
pub struct S(i32);

// This should be rejected because the clone is not affected by the assignment
// to its original.
pub fn main() {
  let mut a = S(1);
  let b = a.clone();
  a.0 = 10;
  assert!(a.0 == b.0)
}
