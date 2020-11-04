extern crate stainless;

// This should fail because of the mut reference parameter.
fn switch_int(r: &mut i32, v: i32) {
  *r = v;
}

fn main() {
  // This should fail because of the mut binding.
  let mut x = 1;
  println!("x = {}", x);

  // This should fail because of the mut borrow.
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
