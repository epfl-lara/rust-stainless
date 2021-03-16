extern crate stainless;

use std::marker::PhantomData;

pub struct Foo<T>(i32, PhantomData<T>);

pub struct Bar<T> {
  a: i32,
  _marker: PhantomData<T>,
}

pub fn main() {
  let foo: Foo<u32> = Foo(-123, PhantomData);

  let bar: Bar<bool> = Bar {
    a: 123,
    _marker: PhantomData,
  };

  assert!(foo.0 == -123);
  assert!(bar.a == 123)
}
