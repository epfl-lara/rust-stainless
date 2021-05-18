#![allow(clippy::new_without_default, clippy::should_implement_trait)]

use im::HashMap;
use std::hash::Hash;

#[derive(Clone)]
pub struct Map<K, V> {
  map: HashMap<K, V>,
}

impl<K, V> Map<K, V>
where
  K: Eq + Hash + Clone,
  V: Clone,
{
  pub fn new() -> Self {
    Self {
      map: HashMap::new(),
    }
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.map.get(key)
  }

  pub fn get_or<'a>(&'a self, key: &K, elze: &'a V) -> &'a V {
    self.get(key).unwrap_or(elze)
  }

  /// Panics if the key is not in the map.
  pub fn index(&self, key: &K) -> &V {
    self.get(key).unwrap()
  }

  pub fn contains_key(&self, key: &K) -> bool {
    self.map.contains_key(key)
  }

  pub fn insert(self, key: K, val: V) -> Self {
    Self {
      map: self.map.update(key, val),
    }
  }

  pub fn remove(self, key: &K) -> Self {
    Self {
      map: self.map.without(key),
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_empty() {
    assert!(!Map::<_, bool>::new().contains_key(&123));
    assert_eq!(Map::<_, u8>::new().get(&456), None);
    assert_eq!(Map::new().get_or(&789, &0), &0);
  }

  #[test]
  fn test_get_removed() {
    let map: Map<String, i32> = Map::new()
      .insert("foo".into(), 1)
      .insert("bar".into(), 2)
      .insert("baz".into(), 3);

    assert_eq!(Some(&1), map.get(&"foo".into()));
    let map = map.remove(&"foo".into());
    assert!(!map.contains_key(&"foo".into()));

    assert_eq!(&3, map.index(&"baz".into()));
    let map = map.insert("bar".into(), 8);
    assert_eq!(&8, map.index(&"bar".into()));
  }

  #[test]
  fn test_get_or_else() {
    assert_eq!(Map::new().get_or(&1, &0), &0);

    let m1 = Map::new().insert(1, 123);
    assert_eq!(m1.get_or(&1, &0), &123);
    assert_eq!(m1.get_or(&0, &-1), &-1);
  }
}
