extern crate stainless;

pub enum IntOption {
  None,
  Some(i32),
}

pub fn unwrap_or_zero(io: IntOption) -> i32 {
  match io {
    IntOption::None => 0,
    IntOption::Some(x) => x,
  }
}

pub fn add_to_int_option(io1: IntOption) -> i32 {
  let io2 = IntOption::None;
  let io3 = IntOption::Some(3);
  unwrap_or_zero(io1) + unwrap_or_zero(io2) + unwrap_or_zero(io3)
}

pub struct TwoIntOptions {
  io1: IntOption,
  io2: IntOption,
}

pub fn get_sum(tio: TwoIntOptions) -> i32 {
  unwrap_or_zero(tio.io1) + unwrap_or_zero(tio.io2)
}

fn main() -> () {}
