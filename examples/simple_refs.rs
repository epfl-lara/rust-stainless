#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Point(i32, i32);
// Also, Point is "freeze" in rustc, i.e. no interior mutability

fn shift_x(p: &mut Point, x: i32) {
  p.0 += x;
}

fn main() {
  let mut p = Point(1, 2);
  shift_x(&mut p, 3);
  assert!(p.0 == 4);
}

/* Rust ==> Stainless

case class Ref[T](v: T)

case class Point(x: Int, y: Int)

def shift_x(p: Ref[Point] @mutable, x: Int): Unit = {
  p.x += x;
}

// Or without Ref[_]:
def shift_x(p: Point, x: Int): Unit = {
  p.x += x;
}

*/