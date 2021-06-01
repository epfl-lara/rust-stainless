extern crate stainless;

pub struct S(i32);

pub fn borrows_mutably<T>(t: &mut T) -> &mut T {
  t
}

pub fn main() {
  let mut a = S(1);
  let b = borrows_mutably(&mut a); // here, no fresh copy is allowed
  b.0 = 100;
  assert!(b.0 == a.0 && a.0 == 100)
}

/*
Desired Scala translation:

import stainless.annotation._

object Mutable {
  final case class MutRef[@mutable T](var t: T)
  final case class S(var s: Int)

  def borrows_mutably[@mutable T](t: MutRef[T]): MutRef[T] = t

  def main() = {
    var a = MutRef(S(1))

    val b = borrows_mutably(a)
    b.t.s = 100

    assert(b.t.s == a.t.s && a.t.s == 100)
  }
}
*/
