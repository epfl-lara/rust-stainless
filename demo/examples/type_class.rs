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

impl<T: Equals> Equals for List<T> {
  /*
  - 'impl<..> Z for X' and has type params => case class
  - impl<tps> => ListEquals[tps]
  - trait bounds => trait bounds implemented by evidence params
  - the trait with as last type param the 'for X' => extends trait[..., X]
  (the last two use the same translation)
  => case class ListEquals[T](ev: Equals[T]) extends Equals[List[T]]
  */

  fn equals(&self, y: &List<T>) -> bool {
    match self {
      List::Nil => match y {
        List::Nil => true,
        _ => false,
      },
      List::Cons(x, xs) => match y {
        List::Cons(y, ys) => x.equals(y) && xs.equals(ys),
        _ => false,
      },
    }
  }
}

/*
(Equals, T, []) -> evidence
(Equals, List, [T]) -> evidence

// ground (always in scope)
(Equals, i32, []) -> IntEquals

// for all things that have type params, possibly add entries to the map (based
// on trait bounds)
//
// Rust already somehow represents this: there may be a list containing the
// bounds `predicates_of`

*/

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
  assert!(!a.equals(&b));

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

// stainless extracts this as follows

 Symbols before PartialFunctions

-------------Functions-------------
@abstract
@method(Equals)
def equals(x: T, y: T): Boolean = <empty tree>[Boolean]

@law
@method(Equals)
def law_transitive(x: T, y: T, z: T): Boolean = ¬(this.equals(x, y) && this.equals(y, z)) || this.equals(x, z)

@law
@method(Equals)
def law_symmetric(x: T, y: T): Boolean = this.equals(x, y) == this.equals(y, x)

@method(IntEquals)
def equals(x: Int, y: Int): Boolean = x == y

@law
@method(Equals)
def law_reflexive(x: T): Boolean = this.equals(x, x)

-------------Classes-------------
@caseObject
case class IntEquals extends Equals[Int]

@abstract
abstract class Equals[T]

*/
