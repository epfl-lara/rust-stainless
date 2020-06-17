#![feature(box_patterns)]

extern crate stainless_contracts as st;

#[st::ensuring(|m| y < x || x < 0 || y < 0 || x <= m && m <= y)]
pub fn mean_wrong(x: i32, y: i32) -> i32 {
    if x >= 0 && y >= 0 {
        (x + y) / 2
    } else {
        -1
    }
}

#[st::ensuring(|m| y < x || x < 0 || y < 0 || x <= m && m <= y)]
pub fn mean_ok(x: i32, y: i32) -> i32 {
    if x >= 0 && y >= 0 {
        x + (y - x) / 2
    } else {
        -1
    }
}

fn main() -> () {}
