extern crate stainless;

fn switch_ref<'a>(rr: &mut &'a i32, v: &'a i32) {
  *rr = v;
}

fn main() {
  let a = 1;
  let b = 2;
  let mut r = &a;
  println!("*r = {}", *r);
  switch_ref(&mut r, &b);
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
