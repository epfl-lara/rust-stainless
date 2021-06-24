extern crate stainless;

#[allow(unused_mut, unused_assignments)]
fn change(mut opt: Option<&mut i32>) {
  if let Some(x) = opt {
    *x = 456;
  }
  // opt = None;
}

pub fn main() {
  let mut x = 123;
  change(None);
  change(Some(&mut x));
  assert!(x == 456)
}

/*

Desired Scala translation:

import stainless.annotation._

object Mutable {
  sealed abstract class Option[@mutable T]
  case class None[@mutable T]() extends Option[T]
  case class Some[@mutable T](_0: MutCell[T]) extends Option[T]

  case class MutCell[@mutable T](var value: T)

  def change(opt: MutCell[Option[MutCell[Int]]]): Unit = {
   (opt.value match {
     case Some(MutCell(x)) => x.value = 456
     case _ => ()
   })
   // opt.value = None[MutCell[Int]]()
  }

  def main: Unit = {
   val x: MutCell[Int] = MutCell[Int](123)
   change(MutCell[Option[MutCell[Int]]](None[MutCell[Int]]()))
   change(MutCell[Option[MutCell[Int]]](Some[MutCell[Int]](MutCell(x))))
   assert(x.value == 456)
  }
}

 */
