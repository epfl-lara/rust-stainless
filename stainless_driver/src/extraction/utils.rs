use std::collections::HashMap;
use std::hash::Hash;

pub type Counter = i32;

/// UniqueCounter provides fresh ids with a name prefix
pub struct UniqueCounter<K: Hash + Eq> {
  next: HashMap<K, Counter>,
}

impl<K: Clone + Hash + Eq> UniqueCounter<K> {
  pub fn new() -> Self {
    Self {
      next: HashMap::new(),
    }
  }

  pub fn fresh(&mut self, k: &K) -> Counter {
    // *self.next.entry(k).and_modify(|c| *c += 1).or_insert(0)
    match self.next.get_mut(k) {
      None => {
        self.next.insert(k.clone(), 1);
        0
      }
      Some(c) => {
        let result = *c;
        *c += 1;
        result
      }
    }
  }
}
