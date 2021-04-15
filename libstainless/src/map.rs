use im::HashMap;
use std::hash::Hash;

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
