extern crate stainless;

fn apply<F: Fn(i32) -> i32>(f: F) -> i32 {
  f(0)
}

fn main() -> () {
  apply(|x| 2 * x);
}

// fn main() -> () {
//   let c = 2;
//   let double = move |x: i32| { let res = c * x; res };
//   double(1);
// }
