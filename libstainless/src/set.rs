use im::HashSet;
use std::hash::Hash;

#[derive(Clone)]
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

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_empty() {
    assert!(!Set::empty().contains(&123));
    assert!(Set::<i32>::empty().is_subset_of(&Set::empty()));
    assert!(Set::empty().is_subset_of(&Set::singleton(123)));
  }

  #[test]
  fn set_theory() {
    let s1 = Set::singleton(1).add(2).add(3);
    let s2 = Set::singleton(1).add(2);
    let s3 = Set::singleton(3).add(4);

    assert!(!s1.is_subset_of(&s2));
    assert!(!s1.is_subset_of(&s3));
    assert!(s2.is_subset_of(&s1));
    assert!(!s3.is_subset_of(&s1));

    let new = s1.union(s3.clone()).difference(s2.clone());
    assert!(new.is_subset_of(&s3) && s3.is_subset_of(&new));
    assert!(s2.intersection(s3).is_subset_of(&Set::empty()))
  }

  #[test]
  fn remove_contains() {
    let set: Set<String> = Set::singleton("foo".into()).add("bar".into());
    let set = set.difference(Set::singleton("bar".into()));
    assert!(!set.contains(&"bar".into()));
    let set = set.difference(Set::singleton("foo".into()));
    assert!(!set.contains(&"foo".into()))
  }
}
