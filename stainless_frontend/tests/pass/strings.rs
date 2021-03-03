extern crate stainless;

pub fn take_string(_s: String) {}

pub fn return_string() -> String {
  "Hello, world".to_string()
}

pub fn take_static_str(_s: &'static str) {}

pub fn return_static_str() -> &'static str {
  "Hello, static world"
}
