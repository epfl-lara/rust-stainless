extern crate stainless;

pub fn main() {
  let a = 456;
  let b = 123;

  assert!(match (a, 123) {
    (a, 23) => a < 10,
    (456, number) => number == b,
    (_, _) => false,
  })
}
