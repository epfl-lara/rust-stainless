use im::HashSet;
use std::hash::Hash;

pub struct Set<T> {
  set: HashSet<T>,
}

impl<T> Set<T>
where
  T: Eq + Hash + Clone,
{
  pub fn empty() -> Self {
    Self {
      set: HashSet::new(),
    }
  }
  pub fn singleton(t: T) -> Self {
    Self {
      set: HashSet::unit(t),
    }
  }

  pub fn add(&self, t: T) -> Self {
    Self {
      set: self.set.update(t),
    }
  }

  pub fn contains(&self, t: &T) -> bool {
    self.set.contains(t)
  }

  pub fn union(self, other: Set<T>) -> Self {
    Self {
      set: self.set.union(other.set),
    }
  }

  pub fn intersection(self, other: Set<T>) -> Self {
    Self {
      set: self.set.intersection(other.set),
    }
  }

  pub fn difference(self, other: Set<T>) -> Self {
    Self {
      set: self.set.difference(other.set),
    }
  }

  pub fn is_subset_of(&self, other: &Set<T>) -> bool {
    self.set.is_subset(&other.set)
  }
}
