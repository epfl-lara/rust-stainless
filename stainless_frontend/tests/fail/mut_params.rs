extern crate stainless;

// This should fail because of the mut ref.
pub fn change<T>(place: &mut T, new: T) -> &mut T {
  *place = new;
  place
}

pub fn main() {
  let mut x = 3;
  let y = 5;

  assert!(x == 3);
  // This should fail because of the mutable reference.
  change(&mut x, y);
  assert!(x == 5)
}
