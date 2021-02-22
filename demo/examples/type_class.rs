extern crate stainless;
use stainless::*;

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

trait Equals {
  // - trait => abstract class
  // - take trait with all type params, add one new at the end
  // => abstract class Equals[T]
  fn equals(&self, x: &Self) -> bool;

  // type_class_insts: Map( ("Equals", "Self", []) -> "this" )
  fn not_equals(&self, x: &Self) -> bool {
    // => this.equals(self, x)
    !self.equals(x)
  }

  #[law]
  fn law_reflexive(x: &Self) -> bool {
    // => this.equals(x, x)
    x.equals(x)
  }

  #[law]
  fn law_symmetric(x: &Self, y: &Self) -> bool {
    x.equals(y) == y.equals(x)
  }

  #[law]
  fn law_transitive(x: &Self, y: &Self, z: &Self) -> bool {
    !(x.equals(y) && y.equals(z)) || x.equals(z)
  }
}

/*
- 'impl<..> Z for X' and has type params => case class
- impl<tps> => ListEquals[tps]
- trait bounds => trait bounds implemented by evidence params
- the trait with as last type param the 'for X' => extends trait[..., X]
(the last two use the same translation)
=> case class ListEquals[T](ev: Equals[T]) extends Equals[List[T]]
*/

impl<T: Equals> Equals for List<T> {
  // #[measure(self)]
  fn equals(&self, other: &List<T>) -> bool {
    match (self, other) {
      (List::Nil, List::Nil) => true,
      (List::Cons(x, xs), List::Cons(y, ys)) => x.equals(y) && xs.equals(ys),
      _ => false,
    }
  }

  fn law_reflexive(x: &Self) -> bool {
    match x {
      List::Cons(x, xs) => T::law_reflexive(x) && Self::law_reflexive(xs),
      List::Nil => true,
    }
  }

  fn law_symmetric(x: &Self, y: &Self) -> bool {
    match (x, y) {
      (List::Cons(x, xs), List::Cons(y, ys)) => {
        T::law_symmetric(x, y) && Self::law_symmetric(xs, ys)
      }
      _ => true,
    }
  }

  fn law_transitive(x: &Self, y: &Self, z: &Self) -> bool {
    match (x, y, z) {
      (List::Cons(x, xs), List::Cons(y, ys), List::Cons(z, zs)) => {
        T::law_transitive(x, y, z) && Self::law_transitive(xs, ys, zs)
      }
      _ => true,
    }
  }
}

// case object IntEquals extends Equals[i32]
impl Equals for i32 {
  fn equals(&self, y: &i32) -> bool {
    // FIXME: deref to act on i32 instead of refs to make sure the primitive
    //   '==' is extracted and not the function 'eq'.
    *self == *y
  }
}

pub fn main() {
  let a = 2;
  let b = 4;

  // => IntEquals.equals(a, b)
  assert!(a.not_equals(&b));

  // => ListEquals.equals(list, list)(IntEquals)
  let list = List::Cons(123, Box::new(List::Cons(456, Box::new(List::Nil))));
  assert!(list.equals(&list));
}

/*
// Desired stainless equivalent of the above:
import stainless.annotation._
import stainless.lang._
import stainless.collections._

abstract class Equals[T] {
  def equals(x: T, y: T): Boolean

  @law
  def law_reflexive(x: T): Boolean = equals(x, x)

  @law
  def law_symmetric(x: T, y: T): Boolean = equals(x, y) == equals(y, x)

  @law
  def law_transitive(x: T, y: T, z: T): Boolean =
    !(equals(x, y) && equals(y, z)) || equals(x, z)
}

case class ListEquals[A](ev: Equals[A]) extends Equals[List[A]] {
  def equals(xs: List[A], ys: List[A]): Boolean = {
    decreases(xs.size)
    (xs, ys) match {
      case (Cons(x, xs), Cons(y, ys)) => ev.equals(x, y) && equals(xs, ys)
      case (Nil(), Nil()) => true
      case _ => false
    }
  }
}

case object IntEquals extends Equals[Int] {
  def equals(x: Int, y: Int): Boolean = x == y
}
*/
