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
/* Already translated tests/functions from Scala (see below).

pub fn size(l: List) -> u32 {
  match l {
    List::Nil => 0,
    List::Cons(_, tail) => 1 + size(*tail),
  }
}

pub fn is_sorted(l: List) -> bool {
  // decreases(l)
  match l {
    List::Nil => true,
    List::Cons(x, tail) => match *tail {
      List::Nil => true,
      List::Cons(y, ys) => x <= y && is_sorted(List::Cons(y, ys)),
    },
  }
}

pub fn min(l: List) -> IntOption {
  // decreases(l)
  match l {
    List::Nil => IntOption::None,
    List::Cons(x, xs) => match min(*xs) {
      IntOption::None => IntOption::Some(x),
      IntOption::Some(y) => {
        if x < y {
          IntOption::Some(x)
        } else {
          IntOption::Some(y)
        }
      }
    },
  }
}

pub fn main() {
  let list = List::Cons(
    12,
    Box::new(List::Cons(3, Box::new(List::Cons(-1, Box::new(List::Nil))))),
  );

  assert!(size(list) == 3);
}

*/

/* Scala tests (not yet translated) copied from
  https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/InsertionSort.scala

  def contents(l: List): Set[Int] = {
    decreases(l)
    l match {
      case Nil() => Set.empty
      case Cons(x,xs) => contents(xs) ++ Set(x)
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
