extern crate stainless;

// This should fail because of the mut ref.
pub fn change<T>(place: &mut T, new: T) -> &mut T {
  *place = new;
  place
}
