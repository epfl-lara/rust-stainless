#![allow(dead_code)]

extern crate stainless;

/// This benchmark stems from [RustVis Specs](http://erik.vestera.as/rustvis/specs).

struct Foo<T> {
  value: Option<T>,
}

struct Bar;

#[allow(unused_variables)]
fn insert<T>(map: &mut Foo<T>, value: T) {}

fn main() {
  let mut map = Foo { value: None };
  {
    let target = Bar;
    let text = &target;

    insert(&mut map, text);
  }

  // potentially more code
}
