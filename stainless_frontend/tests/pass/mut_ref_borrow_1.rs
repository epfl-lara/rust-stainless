extern crate stainless;

pub struct S(i32, i32);

pub fn main() {
  let mut a = S(1, 2);

  let b = &mut a;
  *b = S(100, 200);

  assert!(a.0 == 100 && a.1 == 200)
}

/*
Desired Scala translation:

import stainless.annotation._

object Mutable {
  final case class MutCell[@mutable T](var t: T)
  final case class S(var s1: Int, var s2: Int)

  def main() = {
    val a = MutCell(S(1, 2))

    val b = a
    b.t = S(100, 200)

    assert(a.t.s1 == 100 && a.t.s2 == 200)
  }
}
*/
