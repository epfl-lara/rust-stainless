extern crate stainless;

pub struct Thing<T> {
  field: T,
}

pub fn change_thing<T>(thing: &mut Thing<T>, t: T) {
  *thing = Thing { field: t };
}

pub fn main() {
  let mut thing = Thing { field: 123 };
  change_thing(&mut thing, 456);
  assert!(thing.field == 456);

  let thing2 = &mut thing;
  change_thing(thing2, 789);
  assert!(thing.field == 789);

  let thing3 = &mut thing;
  *thing3 = Thing { field: 0 };
  assert!(thing.field == 0);
}
