extern crate stainless;

fn switch_int(r: &mut i32, v: i32) {
  *r = v;
}

fn main() {
  let mut x = 1;
  println!("x = {}", x);
  switch_int(&mut x, 2);
  println!("x = {}", x);
}

/*

def switch_int(r: i32, v: i32) {
  r = v;
}

def main() {
  var x = 1;
  println!("x = {}", x);
  switch_int(x, 2);
  println!("x = {}", x);
}

*/
