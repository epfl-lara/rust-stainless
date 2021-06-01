extern crate stainless;

pub struct S(i32);

pub fn borrows_mutably<T>(t: &mut T) -> &mut T {
  t
}

pub fn main() {
  let mut a = S(1);
  let b = borrows_mutably(&mut a); // here, no fresh copy is allowed
  b.0 = 100;
  assert!(b.0 == a.0 && a.0 == 100)
}