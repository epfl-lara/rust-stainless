extern crate stainless;

struct S<T>(T);
struct Container<T> {
  s: S<T>,
}

pub fn main() {
  let mut c = Container { s: S("hello") };

  match &mut c.s {
    S(v) if *v == "hello" => *v = "world",
    _ => {}
  };

  assert!(c.s.0 == "world")
}

/*
Desired Scala translation:

import stainless.annotation._

object Fields {
  case class S[@mutable T](_0: MutCell[T])
  case class MutCell[@mutable T](var value: T)
  case class Container[@mutable T](s: MutCell[S[T]])

  def main: Unit = {
    val c: MutCell[Container[String]] = MutCell(Container(MutCell(S(MutCell("hello")))))

    (c.value.s match {
      case MutCell(S(v)) if v.value == "hello" =>
        v.value = "world"
      case _ =>
        ()
    })
    assert(c.value.s.value._0.value == "world")
  }
}

 */
