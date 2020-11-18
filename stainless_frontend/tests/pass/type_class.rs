extern crate stainless;
use stainless::*;

trait Equals {
  fn equals(&self, x: &Self) -> bool;

  #[law]
  fn law_reflexive(x: &Self) -> bool {
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

impl Equals for i32 {
  fn equals(&self, y: &i32) -> bool {
    // FIXME: deref to act on i32 instead of refs to make sure the primitive
    //   '==' is extracted and not the function 'eq'.
    *self == *y
  }
}

pub fn main() {}

/*
// Desired stainless equivalent of the above:
import stainless.annotation._
import stainless.lang._

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
