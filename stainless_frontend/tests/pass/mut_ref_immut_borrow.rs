extern crate stainless;

pub struct S(i32);

pub fn f() {
  let mut a = S(1);

  let c = {
    a.0 = 10;
    a
  };

  assert!(c.0 == 10)
}

pub fn main() {
  let a = S(1);
  let b = S(2);

  // This is actually valid because the borrow in itself is immutable...
  let mut r = &a;
  assert!(match *r {
    S(1) => true,
    _ => false,
  });

  // ... that is reassigned here.
  r = &b;
  assert!(match *r {
    S(2) => true,
    _ => false,
  });
}

/*
This is valid Scala:

def main() = {
  val a = S(1)
  val b = S(2)

  var r = a
  println(r)
  r = b
  println(r)
}
*/
