pub use num_bigint::BigInt;
pub use stainless_macros::*;

mod set;
pub use set::*;

mod map;
pub use map::*;

pub trait Implies {
  fn implies(self, b: Self) -> bool;
}

impl Implies for bool {
  /// Simple helper to provide the implies expression on booleans.
  #[inline(always)]
  fn implies(self, b: bool) -> bool {
    !self || b
  }
}
