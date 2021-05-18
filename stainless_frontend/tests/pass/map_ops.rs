extern crate stainless;
use stainless::*;

/// This test is equivalent to the Scala benchmark:
/// https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/MapDiff.scala
#[pre(m.contains_key(&1) && m.contains_key(&2) && m.contains_key(&3))]
pub fn test(m: &Map<u32, u32>) {
  let m2 = m.clone().remove(&1).remove(&2);
  assert!(!m2.contains_key(&1));
  assert!(!m2.contains_key(&2));
  assert!(m2.contains_key(&3));
  // Deref to do primitive equality of ints
  assert!(*m2.index(&3) == *m.index(&3))
}

#[pre(!a.contains_key(&0))]
#[post(ret)]
pub fn test1(a: &Map<u32, u32>) -> bool {
  let b = a.clone().insert(0, 1);
  let c = a.clone().insert(0, 1);
  // Deref to do primitive equality of ints
  *b.index(&0) == *c.index(&0)
}

#[pre(!a.contains_key(&0))]
#[post(ret == -123)]
pub fn test3(a: &Map<u32, i32>) -> i32 {
  let b = *a.get_or(&0, &-123);
  // Deref to do primitive equality of ints
  assert!(b == *Map::new().get_or(&0, &-123));
  b
}

#[pre(!a.contains_key(&0))]
#[post(matches!(ret, None))]
pub fn test2(a: &Map<u32, u32>) -> Option<&u32> {
  a.get(&0)
}

pub fn main() {}
