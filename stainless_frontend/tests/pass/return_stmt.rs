extern crate stainless;
use stainless::*;

pub enum Option<T> {
  None,
  Some(T),
}

#[pre(x >= 0 && x < 10)]
#[post(ret >= 0)]
pub fn fact(x: i32) -> i32 {
  if x <= 0 {
    return 1;
  }

  fact(x - 1) * x
}

pub fn return_when_wanted(arg: u32) {
  if arg < 10 {
    return;
  }
  let a = 1;
  let useless = 3;
  let computation = useless + a;

  if computation > 3 {
    return;
  }

  panic!("will never reach me")
}

pub fn return_pattern_match(arg: u32) -> i32 {
  let s = match arg {
    1 => return 1,
    i if 3 <= i && i <= 10 => 100000,
    a => {
      if a > 1000 {
        return 2;
      }
      10000000
    }
  };
  assert!(s >= 100000);
  s
}

pub fn some_option_fn(option: &Option<i32>, v: i32) -> bool {
  match option {
    Option::None => {}
    Option::Some(x) => return *x == v,
  };
  false
}

pub fn flatten<T>(opt: Option<Option<T>>) -> Option<T> {
  match opt {
    Option::Some(maybe) => return maybe,
    _ => {}
  }
  return Option::None;
}

#[allow(unused_variables)]
fn foo(x: Option<i32>) -> i32 {
  match x {
    Option::Some(i) if return 0 => i,
    Option::Some(_) => 42,
    Option::None => -42,
  }
}

fn bar() -> i32 {
  if 12 == 12 && return 0 {
    return 1;
  } else {
    return 2;
  }
}

pub fn main() {
  assert!(foo(Option::Some(123)) == 0);
  assert!(foo(Option::None) == -42);
  assert!(bar() == 0)
}
