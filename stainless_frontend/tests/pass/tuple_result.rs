extern crate stainless;

pub fn main() {
  let a: Result<(), i32> = Ok(());
  let b: Result<(), i32> = Err(123);

  assert!(matches!(a, Ok(())));
  assert!(matches!(b, Err(123)))
}
