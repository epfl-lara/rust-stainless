extern crate stainless;

struct BigStruct {
  a: i64,
  b: i16,
  c: i32,
  d: i64,
  e: i64,
}

pub fn main() {
  let s = BigStruct {
    a: 1,
    b: 2,
    c: 3,
    d: 4,
    e: -5,
  };

  let t = BigStruct { a: 6, d: 789, ..s };

  assert!(t.a == 6 && t.b == 2 && t.c == 3 && t.d == 789 && t.e == -5)
}
