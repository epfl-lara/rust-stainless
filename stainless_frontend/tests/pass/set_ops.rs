extern crate stainless;
use stainless::*;

/// This test is equivalent to the Scala benchmark:
/// https://github.com/epfl-lara/stainless/blob/master/frontends/benchmarks/verification/valid/MySet.scala

#[post(ret)]
pub fn set1() -> bool {
  let s = Set::singleton(1).insert(2).insert(3).insert(4);
  s.contains(&3)
}

#[post(ret)]
pub fn set2() -> bool {
  let s1 = Set::new().insert(1);
  let s2 = Set::singleton(1);
  s1.is_subset(&s2) && s2.is_subset(&s1)
}
