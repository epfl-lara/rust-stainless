extern crate stainless;

pub enum Option<T> {
  None,
  Some(T),
}

pub fn unwrap_or_default<T>(o: Option<T>, default: T) -> T {
  match o {
    Option::None => default,
    Option::Some(x) => x,
  }
}

pub fn add_to_int_option(io1: Option<i32>) -> i32 {
  let io2 = Option::None;
  let io3 = Option::Some(3);
  unwrap_or_default(io1, 0) + unwrap_or_default(io2, 0) + unwrap_or_default(io3, 0)
}

pub struct TwoOptions<T> {
  io1: Option<T>,
  io2: Option<T>,
}

pub fn get_sum(tio: TwoOptions<i32>) -> i32 {
  unwrap_or_default(tio.io1, 0) + unwrap_or_default(tio.io2, 0)
}
