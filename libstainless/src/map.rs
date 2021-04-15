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
  pub fn empty() -> Self {
    Self {
      map: HashMap::new(),
    }
  }

  pub fn get(&self, key: &K) -> Option<&V> {
    self.map.get(key)
  }

  pub fn get_or_else<'a>(&'a self, key: &K, elze: &'a V) -> &'a V {
    self.get(key).unwrap_or(elze)
  }

  /// Panics if the key is not in the map.
  pub fn apply(&self, key: &K) -> &V {
    self.get(key).unwrap()
  }

  pub fn contains(&self, key: &K) -> bool {
    self.map.contains_key(key)
  }

  pub fn updated(&self, key: K, val: V) -> Self {
    Self {
      map: self.map.update(key, val),
    }
  }

  pub fn removed(&self, key: &K) -> Self {
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
    assert!(!Map::<_, bool>::empty().contains(&123));
    assert_eq!(Map::<_, u8>::empty().get(&456), None);
    assert_eq!(Map::empty().get_or_else(&789, &0), &0);
  }

  #[test]
  fn test_get_removed() {
    let map: Map<String, i32> = Map::empty()
      .updated("foo".into(), 1)
      .updated("bar".into(), 2)
      .updated("baz".into(), 3);

    assert_eq!(Some(&1), map.get(&"foo".into()));
    let map = map.removed(&"foo".into());
    assert!(!map.contains(&"foo".into()));

    assert_eq!(&3, map.apply(&"baz".into()));
    let map = map.updated("bar".into(), 8);
    assert_eq!(&8, map.apply(&"bar".into()));
  }

  #[test]
  fn test_get_or_else() {
    assert_eq!(Map::empty().get_or_else(&1, &0), &0);

    let m1 = Map::empty().updated(1, 123);
    assert_eq!(m1.get_or_else(&1, &0), &123);
    assert_eq!(m1.get_or_else(&0, &-1), &-1);
  }
}
