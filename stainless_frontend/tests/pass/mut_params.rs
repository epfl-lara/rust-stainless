extern crate stainless;

pub fn change<T>(place: &mut T, new: T) -> &mut T {
  *place = new;
  place
}
