extern crate stainless;

pub fn id<T>(x: T) -> T {
  x
}

fn main() -> () {
  id(0);
}
