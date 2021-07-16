extern crate stainless;

#[derive(Clone)]
pub struct S(i32);

pub fn main() {
  let mut a = S(1);
  let ref_a = &mut a;
  let b = ref_a.clone();
  ref_a.0 = 10;
  assert!(a.0 != b.0);
}
