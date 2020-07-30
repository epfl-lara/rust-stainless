extern crate stainless;
use stainless::*;

struct State(i32);

#[var(0)]
struct Printer(i32);

#[pure(state)]
#[external]
fn do_sideeffectful_stuff(state: State, _printer: Printer) -> i32 {
  state.0
}

#[pre(state.0 == 123)]
#[post(state.0 == 123)]
fn work(state: State, printer: Printer) -> i32 {
  do_sideeffectful_stuff(state, printer)
}

fn main() -> () {
  work(State(123), Printer(999));
}
