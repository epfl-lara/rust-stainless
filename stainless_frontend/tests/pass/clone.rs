extern crate stainless;
use stainless::*;

#[derive(Clone)]
pub struct Key(String);

#[derive(Clone)]
pub struct Door {
  key_lock: Key,
}

impl Door {
  fn try_open(&self, key: &Key) -> Result<(), &'static str> {
    if key.0 == self.key_lock.0 {
      Ok(())
    } else {
      Err("can't open that door")
    }
  }
}

#[derive(Clone)]
pub struct Person {
  key: Key,
}

fn do_clone<T: Clone>(t: &T) -> T {
  t.clone()
}

// If the person has the key of the door, it implies that they can open it.
#[post((person.key.0 == door.key_lock.0).implies(matches!(ret, Ok(_))))]
pub fn main(door: Door, person: Person) -> Result<(), &'static str> {
  door.try_open(&do_clone(&person.key))
}
