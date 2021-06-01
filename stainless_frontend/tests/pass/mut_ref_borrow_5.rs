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

/*
Desired Scala translation:

import stainless.lang._
import stainless.annotation._

object Mutable {
  final case class MutRef[@mutable T](var t: T)
  final case class Thing[@mutable T](var field: T)

  def change_thing[@mutable T](thing: MutRef[Thing[T]], t: T) = {
    thing.t = Thing(freshCopy(t))
  }

  def main() = {
    val thing = MutRef(Thing(123))
    change_thing(thing, 456)
    assert(thing.t.field == 456)

    val thing2 = thing
    change_thing(thing2, 789)
    assert(thing.t.field == 789)

    val thing3 = thing
    thing3.t = Thing(0)
    assert(thing.t.field == 0)
  }
}
*/
