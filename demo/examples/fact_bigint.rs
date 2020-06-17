extern crate num_bigint;
use num_bigint::BigInt;

pub fn fact(x: BigInt) -> BigInt {
  if x <= 0.into() {
    1.into()
  } else {
    fact(x.clone() - 1) * x
  }
}

pub fn fact_tail(x: BigInt, acc: BigInt) -> BigInt {
  if x <= 0.into() {
    acc
  } else {
    fact_tail(x.clone() - 1, acc * x)
  }
}

fn main() -> () {}
