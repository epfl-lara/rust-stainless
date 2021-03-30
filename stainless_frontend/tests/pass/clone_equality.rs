extern crate stainless;
use stainless::*;

trait Equals {
  fn eq(&self, other: &Self) -> bool;
}

trait Clone {
  fn clone(&self) -> Self;
}

#[external]
#[pure]
#[post(x.clone().eq(&x))]
#[allow(unused_variables)]
fn clone_preserves_eq<T: Clone + Equals>(x: T) {}

pub struct Key(String);
impl Clone for Key {
  fn clone(&self) -> Self {
    Key(self.0.clone())
  }
}
impl Equals for Key {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}

pub struct Door {
  key_lock: Key,
}

impl Door {
  fn try_open(&self, key: &Key) -> Result<(), &'static str> {
    if key.eq(&self.key_lock) {
      Ok(())
    } else {
      Err("can't open that door")
    }
  }
}

pub struct Person {
  key: Key,
}

// If the person has the key of the door, it implies that they can open it.
#[post(!(person.key.eq( &door.key_lock)) || matches!(ret, Ok(output)))]
pub fn main(door: Door, person: Person) -> Result<(), &'static str> {
  door.try_open(&person.key.clone())
}
