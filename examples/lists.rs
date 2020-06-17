#![no_main]
#![feature(box_patterns)]

extern crate num_bigint;
use num_bigint::BigInt;

extern crate stainless_macros as st;

// #[derive(Clone)]
pub enum IntList {
    Nil,
    Cons(i32, Box<IntList>)
}

pub fn is_sorted(list: IntList) -> bool {
    match list {
        IntList::Cons(a, box IntList::Cons(b, rest)) =>
            a <= b && is_sorted(IntList::Cons(b, rest)),
        _ => true
    }
}

pub fn is_empty(list: IntList) -> bool {
    match list {
        IntList::Nil => true,
        IntList::Cons(_, _) => false
    }
}

#[st::ensuring(|res| is_empty(list) || res > 0.into())]
pub fn size(list: IntList) -> BigInt {
    match list {
        IntList::Nil => 0.into(),
        IntList::Cons(_, rest) => 1 + size(*rest)
    }
}

// #[st::ensuring(|res: IntList| {
//     !is_sorted(list.clone()) || is_sorted(res.clone()) && size(res) == size(list) + 1.into()
// })]
#[st::ensuring(|res| !is_sorted(list) || is_sorted(res))]
pub fn insert(e: i32, list: IntList) -> IntList {
    match list {
        IntList::Nil => IntList::Cons(e, Box::new(IntList::Nil)),
        IntList::Cons(x, xs) => if x <= e {
            IntList::Cons(x, Box::new(insert(e, *xs)))
        } else {
            IntList::Cons(e, Box::new(IntList::Cons(x, xs)))
        }
    }
}

#[st::ensuring(|res| size(res) == size(l1) + size(l2))]
pub fn append(l1: IntList, l2: IntList) -> IntList {
    match l1 {
        IntList::Nil => l2,
        IntList::Cons(x, rest) => IntList::Cons(x, Box::new(append(*rest, l2)))
    }
}
