fn foo(_n: i32) -> () {}

pub fn bar(x: i32) -> i32 {
  foo(x);
  let y = x * 2;
  foo(y);
  if y <= 0 {
    1
  } else {
    bar(x - 1) * x
  }
}

fn main() -> () {}
