extern crate stainless;

fn main() {
  let a = 1;
  let b = 2;

  // This should fail because of the mut reference,
  let mut r = &a;
  println!("*r = {}", *r);

  // that is modified here.
  r = &b;
  println!("*r = {}", *r);
}

/*

def switch_ref(rr: i32, v: i32) {
  rr = v;
}

def main() {
  val a = 1;
  val b = 2;
  var r = a;
  println!("*r = {}", r);   // 1
  switch_ref(r, b);         // No side-effect !!!
  println!("*r = {}", r);   // 1              !!!
}

*/
