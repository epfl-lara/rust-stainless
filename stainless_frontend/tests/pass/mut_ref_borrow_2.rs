extern crate stainless;

pub struct S(i32);

pub fn main() {
  let mut a = S(1);
  let b = {
    a.0 = 10;
    &mut a // here, no fresh copy is allowed
  };
  b.0 = 100;
  assert!(b.0 == a.0 && a.0 == 100)
}

/*
Desired Scala translation:

import stainless.annotation._

object Mutable {
  final case class MutRef[@mutable T](var t: T)
  final case class S(var s: Int)

  def main() = {
    var a = MutRef(S(1))

    val b = {
      a.t.s = 10
      a
    }
    b.t.s = 100

    assert(b.t.s == a.t.s && a.t.s == 100)
  }
}
*/
