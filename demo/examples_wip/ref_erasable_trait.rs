#![allow(unused_mut, unused_variables)]
extern crate stainless;
use stainless::RefErasable;

fn check_ref_erasable<T: RefErasable>(x: T) {}

fn main() {
  let mut v = 1 as i32;
  let mut v_ = 1 as i32;
  let mut v__ = 1 as i32;
  let mut rv = &v;
  let mut rv_ = &v;
  let mut rmv = &mut v_;
  let mut rmv_ = &mut v__;
  let mut rrv = &rv;
  let mut rrmv = &rmv;
  let mut rmrv = &mut rv_;
  let mut rmrmv = &mut rmv_;

  // RefErasable
  check_ref_erasable(v);
  check_ref_erasable(rv);
  check_ref_erasable(rrv);

  // !RefErasable
  // check_ref_erasable(rmv);
  // check_ref_erasable(rrmv);
  // check_ref_erasable(rmrv);
  // check_ref_erasable(rmrmv);
}
