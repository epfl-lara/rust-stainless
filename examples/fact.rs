pub fn fact(x: i32) -> i32 {
  if x <= 0 {
    1
  } else {
    fact(x - 1) * x
  }
}
