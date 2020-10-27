extern crate stainless;

// In rustc's HIR all of the following make_* functions contain somewhat
// distinct trees. Here's a succinct overview of the cases I think we'd like to
// consider in the short term:
//
//                     Struct    Enum
//                   +-------------------
// ExprKind::Path    | foo1      bar1
// ExprKind::Call    | foo2      bar2
// ExprKind::Struct  | foo3      bar3

pub struct Foo1;

pub struct Foo2(i32, bool);

pub struct Foo3 {
  a: i32,
  b: bool,
}

pub enum Bar {
  Bar1,
  Bar2(i32, bool),
  Bar3 { c: i32, d: bool },
}

fn make_foo1() -> Foo1 {
  Foo1
}

fn make_foo2() -> Foo2 {
  Foo2(0, false)
}

fn make_foo3() -> Foo3 {
  Foo3 { a: 0, b: false }
}

fn make_bar1() -> Bar {
  Bar::Bar1
}

fn make_bar2() -> Bar {
  Bar::Bar2(0, false)
}

fn make_bar3() -> Bar {
  Bar::Bar3 { c: 0, d: false }
}

fn get_i32_from_bar(bar: Bar) -> i32 {
  match bar {
    Bar::Bar1 => 0,
    Bar::Bar2(c, _) => c,
    Bar::Bar3 { c, .. } => c,
  }
}

fn get_bool_from_bar(bar: Bar) -> bool {
  match bar {
    Bar::Bar1 => false,
    Bar::Bar2(_, d) => d,
    Bar::Bar3 { d, .. } => d,
  }
}

pub fn main() -> () {
  match make_foo1() {
    Foo1 => (),
  };

  let foo2 = make_foo2();
  foo2.0;
  foo2.1;
  match foo2 {
    Foo2(a, b) => (a, b),
  };

  let foo3 = make_foo3();
  foo3.a;
  foo3.b;
  match foo3 {
    Foo3 { a, b } => (a, b),
  };

  get_i32_from_bar(make_bar1());
  get_bool_from_bar(make_bar2());
  make_bar3();
}
