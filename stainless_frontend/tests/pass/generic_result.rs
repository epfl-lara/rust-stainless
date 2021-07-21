extern crate stainless;
use stainless::*;

// These are just here, because the bug was triggered more easily with more
// stuff to extract.
trait Clone {
  fn clone(&self) -> Self;
}
trait Default {
  fn default() -> Self;
}
pub trait Equals {
  fn eq(&self, other: &Self) -> bool;
}

pub enum Result<T, E> {
  Ok(T),
  Err(E),
}

impl<T, E> Result<T, E> {
  #[post(matches!(self, Result::Ok(_)) == ret)]
  pub fn is_ok(&self) -> bool {
    match self {
      Result::Ok(_) => true,
      _ => false,
    }
  }
}

pub fn main() {}
