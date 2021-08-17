#![allow(dead_code)]

extern crate stainless;

struct Container<K, V> {
  pub pair: Option<(K, V)>,
}

impl<K, V> Container<K, V> {
  pub fn new() -> Self {
    Container { pair: None }
  }

  pub fn insert(&mut self, k: K, v: V) {
    self.pair = Some((k, v))
  }
}

fn main() {
  let mut target = Container::new();
  let key = "foo";

  target.insert(key, 5);

  let value = match &target.pair {
    Some((_, v)) => v,
    _ => panic!("no value"),
  };

  assert!(*value == 5)
}
