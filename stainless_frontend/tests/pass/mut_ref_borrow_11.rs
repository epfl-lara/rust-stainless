extern crate stainless;

#[allow(unused_mut, unused_assignments)]
fn change(mut opt: Option<&mut i32>) {
  if let Some(x) = opt {
    *x = 456;
  }
  opt = None;
}

pub fn main() {
  let mut x = 123;
  change(None);
  change(Some(&mut x));
  assert!(x == 456)
}
