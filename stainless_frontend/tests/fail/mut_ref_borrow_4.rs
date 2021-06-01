extern crate stainless;

/// This examples shows that even if a struct contains a mutable reference, as
/// long as the struct itself is immutably borrowed, we cannot change any data
/// reachable from it.

// This currently fails because we don't extract lifetimes.
struct X<'a>(&'a mut i32);

fn main() {
  let mut int = 123;
  let mut x = X(&mut int);

  let y: &X = &x;
  assert!(*y.0 == 123 && *x.0 == 123);
  // *y.0 = 456; <- this is an error

  let z: &mut X = &mut x;
  assert!(*z.0 == 123);

  *z.0 = 789;
  assert!(*z.0 == 789);
  assert!(*x.0 == 789);
  assert!(int == 789)
}
