extern crate stainless;

pub fn main() {
  let mut x = Some(32);
  match x {
    Some(ref mut y) => *y = 0,
    None => (),
  }
  assert!(matches!(x, Some(0)))
}
