extern crate stainless;

pub fn abs(x: i32) -> i32 {
  if x >= 0 {
    x
  } else {
    -x
  }
}

pub fn abs_pair(x: i32) -> (i32, i32) {
  (x, abs(x))
}

pub fn swap(t: (i32, i32)) -> (i32, i32) {
  (t.1, t.0)
}

// pub fn fst((x, _): (i32, i32)) -> i32 { x }  // TODO
// pub fn snd((_, y): (i32, i32)) -> i32 { y }  // TODO

fn main() -> () {}
