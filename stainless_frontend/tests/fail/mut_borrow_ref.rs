extern crate stainless;

// This should fail because we disallow the mut reference type.
fn switch_int(r: &mut i32, v: i32) {
  *r = v;
}

pub fn main() {
  let mut x = 1;
  assert!(x == 1);

  // This should fail because we disallow mutable borrow.
  switch_int(&mut x, 2);
  assert!(x == 2);
}
