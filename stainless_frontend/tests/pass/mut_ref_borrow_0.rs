extern crate stainless;

pub fn main() {
  let mut x = 1;

  let y = &mut x;
  assert!(*y == 1);
  *y = 2;

  assert!(x == 2)
}

/*
Desired Scala translation:

import stainless.annotation._

object Mutable {
  final case class MutCell[@mutable T](var t: T)

  def main() = {
    val x = MutCell(1)

    val y = x
    assert(y.t == 1)
    y.t = 2

    assert(x.t == 2)
  }
}
*/
