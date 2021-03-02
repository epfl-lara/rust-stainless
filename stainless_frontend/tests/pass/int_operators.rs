extern crate stainless;

pub fn i32_ops(x: i32, y: i32) {
  assert!(x + y == y + x);
  assert!(x + x == 2 * x);
  assert!(x + x == x << 1);
  if x >= 0 && x < 1 << 30 {
    assert!(x == (x + x) / 2);
  }

  assert!(x - y == -y + x);
  assert!(x - x == 0);

  assert!(x * y == y * x);
  assert!(x == 0 || x % x == 0);

  if x > 0 && x < 128 && y >= 0 && y <= 128 {
    assert!((x * y) % x == 0);
  }

  assert!(x | y == y | x);
  assert!(x | x == x);

  assert!(x & y == y & x);
  assert!(x & x == x);
  assert!(x & 0 == 0);

  assert!(x ^ y == y ^ x);
  assert!(x ^ x == 0);

  // FIXME: Enable once missing case in Stainless typechecker is added
  // assert!(x == !!x);
}

pub fn isize_ops(x: isize, y: isize) {
  assert!(x + y == y + x);
  assert!(x + x == 2 * x);
  assert!(x + x == x << 1);
  if x >= 0 && x < 1 << 30 {
    assert!(x == (x + x) / 2);
  }

  assert!(x - y == -y + x);
  assert!(x - x == 0);

  assert!(x * y == y * x);
  assert!(x == 0 || x % x == 0);

  if x > 0 && x < 128 && y >= 0 && y <= 128 {
    assert!((x * y) % x == 0);
  }

  assert!(x | y == y | x);
  assert!(x | x == x);

  assert!(x & y == y & x);
  assert!(x & x == x);
  assert!(x & 0 == 0);

  assert!(x ^ y == y ^ x);
  assert!(x ^ x == 0);

  // FIXME: Enable once missing case in Stainless typechecker is added
  // assert!(x == !!x);
}

pub fn u32_ops(x: u32, y: u32) {
  assert!(x + y == y + x);
  assert!(x + x == 2 * x);
  assert!(x + x == x << 1u32);
  assert!(x >= 1 << 31u32 || x == (x + x) / 2);

  assert!(x - x == 0);

  assert!(x * y == y * x);
  assert!(x == 0 || x % x == 0);

  assert!(x | y == y | x);
  assert!(x | x == x);

  assert!(x & y == y & x);
  assert!(x & x == x);
  assert!(x & 0 == 0);

  assert!(x ^ y == y ^ x);
  assert!(x ^ x == 0);

  // FIXME: Enable once missing case in Stainless typechecker is added
  // assert!(x == !!x);
  // assert!(x - y == !y + x);
}

pub fn usize_ops(x: usize, y: usize) {
  assert!(x + y == y + x);
  assert!(x + x == 2 * x);
  assert!(x + x == x << 1usize);
  assert!(x >= 1 << 31usize || x == (x + x) / 2);

  assert!(x - x == 0);

  assert!(x * y == y * x);
  assert!(x == 0 || x % x == 0);

  assert!(x | y == y | x);
  assert!(x | x == x);

  assert!(x & y == y & x);
  assert!(x & x == x);
  assert!(x & 0 == 0);

  assert!(x ^ y == y ^ x);
  assert!(x ^ x == 0);

  // FIXME: Enable once missing case in Stainless typechecker is added
  // assert!(x == !!x);
  // assert!(x - y == !y + x);
}

// Test the widening of shift operands

pub fn shift_operand_u8_8(x: u8, y: u8) -> u8 {
  x << y
}

pub fn shift_operand_u16_8(x: u16, y: u8) -> u16 {
  x << y
}
pub fn shift_operand_u16_16(x: u16, y: u16) -> u16 {
  x << y
}

pub fn shift_operand_u32_8(x: u32, y: u8) -> u32 {
  x << y
}
pub fn shift_operand_u32_16(x: u32, y: u16) -> u32 {
  x << y
}
pub fn shift_operand_u32_32(x: u32, y: u32) -> u32 {
  x << y
}

pub fn shift_operand_u64_8(x: u64, y: u8) -> u64 {
  x >> y
}
pub fn shift_operand_u64_16(x: u64, y: u16) -> u64 {
  x >> y
}
pub fn shift_operand_u64_32(x: u64, y: u32) -> u64 {
  x >> y
}
pub fn shift_operand_u64_64(x: u64, y: u64) -> u64 {
  x >> y
}

pub fn shift_operand_i8_8(x: i8, y: i8) -> i8 {
  x << y
}

pub fn shift_operand_i16_8(x: i16, y: i8) -> i16 {
  x << y
}
pub fn shift_operand_i16_16(x: i16, y: i16) -> i16 {
  x << y
}

pub fn shift_operand_i32_8(x: i32, y: i8) -> i32 {
  x << y
}
pub fn shift_operand_i32_16(x: i32, y: i16) -> i32 {
  x << y
}
pub fn shift_operand_i32_32(x: i32, y: i32) -> i32 {
  x << y
}

pub fn shift_operand_i64_8(x: i64, y: i8) -> i64 {
  x >> y
}
pub fn shift_operand_i64_16(x: i64, y: i16) -> i64 {
  x >> y
}
pub fn shift_operand_i64_32(x: i64, y: i32) -> i64 {
  x >> y
}
pub fn shift_operand_i64_64(x: i64, y: i64) -> i64 {
  x >> y
}
