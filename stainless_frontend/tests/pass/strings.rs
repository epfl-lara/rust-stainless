extern crate stainless;
use stainless::*;

fn take_string(_s: String) {}

fn return_string() -> String {
  "Hello, world".to_string()
}

fn take_static_str(_s: &'static str) {}
fn take_str<'a>(_s: &'a str) {}

fn return_static_str() -> &'static str {
  "Hello, static world"
}

pub fn test_eq() {
  assert!(return_string() == return_string());
  // != would be extracted as `PartialEq::ne` which is not supported.
  assert!(!(return_string() == return_static_str()));

  take_string(return_string());
  take_string(return_static_str().to_string());
  take_static_str(return_static_str());
  take_str(return_static_str());
}

#[allow(unused_variables)]
#[pre(s.len() > 0)]
fn need_non_empty(s: &str) {
  assert!(s.len() != 0);
}
pub fn provide_non_empty(s: &str) {
  if s.len() > 10 {
    need_non_empty(s);
  }
}
