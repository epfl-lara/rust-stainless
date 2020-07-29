extern crate stainless;

struct Foo;

impl Foo {
  fn new() -> Self {
    Self
  }

  fn get(&self) -> i32 {
    1
  }
}

pub fn f() -> i32 {
  let foo = Foo::new();
  foo.get()
}

fn main() -> () {}
