extern crate stainless;

/// This examples shows that even if a struct contains a mutable reference, as
/// long as the struct itself is immutably borrowed, we cannot change any data
/// reachable from it.

// This currently fails because we don't extract lifetimes.
struct X<'a>(&'a mut i32);

fn main() {
  let mut int = 123;
  let mut x = X(&mut int);

  let y: &X = &x;
  assert!(*y.0 == 123 && *x.0 == 123);
  // *y.0 = 456; <- this is an error

  let z: &mut X = &mut x;
  assert!(*z.0 == 123);

  *z.0 = 789;
  assert!(*z.0 == 789);
  assert!(*x.0 == 789);
  assert!(int == 789)
}

/*
Desired Scala translation:

import stainless.annotation._

object Mutable {
  final case class MutRef[@mutable T](var t: T)
  final case class S(var s: MutRef[Int])

  def main() = {
    val int = MutRef(123)
    val x = MutRef(S(int))

    val y = x
    assert(y.t.s.t == 123 && x.t.s.t == 123)

    val z = x
    assert(z.t.s.t == 123)

    z.t.s.t = 789
    assert(z.t.s.t == 789)
    assert(x.t.s.t == 789)
    assert(int.t == 789)
  }
}
*/
