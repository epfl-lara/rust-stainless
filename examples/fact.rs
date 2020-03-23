// use std::fmt::Formatter;

pub fn fact(x: i32) -> i32 {
  if x <= 0 {
    1
  } else {
    fact(x - 1) * x
  }
}

pub fn fact_pair(x: i32) -> (i32, i32) {
  (x, fact(x))
}

pub fn swap(t: (i32, i32)) -> (i32, i32) {
  (t.1, t.0)
}
