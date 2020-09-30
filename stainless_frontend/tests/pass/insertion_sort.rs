extern crate stainless;
use stainless::*;

pub enum List {
  Nil,
  Cons(i32, Box<List>),
}

pub enum IntOption {
  None,
  Some(i32),
}

/* Already translated size function

#[post(ret >= 0)]
pub fn size(l: &List) -> i32 {
  match l {
    List::Nil => 0,
    List::Cons(_, tail) => 1 + size(tail),
  }
}

#[test]
fn main() {
  let list = List::Cons(
    12,
    Box::new(List::Cons(3, Box::new(List::Cons(-1, Box::new(List::Nil))))),
  );

  assert_eq!(size(&list), 3)
}
 */

/* Scala test copied from
  https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/InsertionSort.scala

  def contents(l: List): Set[Int] = {
    decreases(l)
    l match {
      case Nil() => Set.empty
      case Cons(x,xs) => contents(xs) ++ Set(x)
    }
  }

  def min(l : List) : OptInt = {
    decreases(l)
    l match {
      case Nil() => None()
      case Cons(x, xs) => min(xs) match {
        case None() => Some(x)
        case Some(x2) => if(x < x2) Some(x) else Some(x2)
      }
    }
  }

  def isSorted(l: List): Boolean = {
    decreases(l)
    l match {
      case Nil() => true
      case Cons(x, Nil()) => true
      case Cons(x, Cons(y, ys)) => x <= y && isSorted(Cons(y, ys))
    }
  }

  /* Inserting element 'e' into a sorted list 'l' produces a sorted list with
   * the expected content and size */
  def sortedIns(e: Int, l: List): List = {
    require(isSorted(l))
    decreases(l)
    l match {
      case Nil() => Cons(e,Nil())
      case Cons(x,xs) => if (x <= e) Cons(x,sortedIns(e, xs)) else Cons(e, l)
    }
  } ensuring(res => contents(res) == contents(l) ++ Set(e)
                    && isSorted(res)
                    && size(res) == size(l) + 1
            )

  /* Insertion sort yields a sorted list of same size and content as the input
   * list */
  def sort(l: List): List = {
    decreases(l)
    l match {
      case Nil() => Nil()
      case Cons(x,xs) => sortedIns(x, sort(xs))
    }
  } ensuring(res => contents(res) == contents(l)
                     && isSorted(res)
                     && size(res) == size(l)
             )

  @extern
  def main(args: Array[String]): Unit = {
    val ls: List = Cons(5, Cons(2, Cons(4, Cons(5, Cons(1, Cons(8,Nil()))))))

    println(ls)
    println(sort(ls))
  }
}
 */
