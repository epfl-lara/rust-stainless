extern crate stainless;

/// This benchmark stems from [RustVis Specs](http://erik.vestera.as/rustvis/specs)
/// and was adapted to work with stainless.

struct Container<K, V> {
  pair: Option<(K, V)>,
}

impl<K, V> Container<K, V> {
  pub fn new() -> Self {
    Container { pair: None }
  }

  pub fn insert(&mut self, k: K, v: V) {
    self.pair = Some((k, v))
  }
}

impl<V> Container<String, V> {
  pub fn get_mut(&mut self, key: &String) -> Option<&mut V> {
    match &mut self.pair {
      Some((k, v)) if *k == *key => Some(v),
      _ => None,
    }
  }
}

pub fn main() {
  let mut target = Container::new();
  let key = "foo".to_string();

  let ref1 = &mut target;
  match ref1.get_mut(&key) {
    Some(value) => *value = 123,
    _ => {
      target.insert(key.clone(), 5);
      match target.get_mut(&key) {
        Some(v) => *v = 123,
        _ => panic!("no value"),
      }
    }
  }

  assert!(matches!(
    target,
    Container {
      pair: Some((key, 123))
    } if key == "foo"
  ))
}
