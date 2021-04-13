extern crate stainless;
use stainless::*;

/// This test is equivalent to the Scala benchmark:
/// https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/MapDiff.scala
#[pre(m.contains(&1) && m.contains(&2) && m.contains(&3))]
pub fn test(m: &Map<u32, u32>) {
  let m2 = m.removed(&1).removed(&2);
  assert!(!m2.contains(&1));
  assert!(!m2.contains(&2));
  assert!(m2.contains(&3));
  assert!(*m2.apply(&3) == *m.apply(&3))
}

#[pre(!a.contains(&0))]
#[post(ret)]
pub fn test1(a: &Map<u32, u32>) -> bool {
  let b = a.updated(&0, &1);
  let c = a.updated(&0, &1);
  // Deref to do primitive equality of ints
  *b.apply(&0) == *c.apply(&0)
}

#[pre(!a.contains(&0))]
#[post(matches!(ret, None))]
pub fn test2(a: &Map<u32, u32>) -> Option<&u32> {
  a.get(&0)
}

#[pre(!a.contains(&0))]
#[post(ret)]
pub fn test3(a: &Map<u32, u32>) -> bool {
  a.get(&0) == Map::empty().get(&0)
}
