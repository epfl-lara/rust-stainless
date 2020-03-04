// use std::fmt::Formatter;

pub fn fact(x: i32) -> i32 {
  fn bla() -> () { () }

  println!("hi");

  let y = x;

  if y <= 0 {
    1
  } else {
    fact(x - 1) * x
  }
}
