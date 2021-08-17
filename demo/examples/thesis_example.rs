extern crate stainless;
use stainless::*;

trait Id {
  /// Calculate ID for this object.
  fn id(&self) -> isize;

  #[law]
  fn law_positive(&self) -> bool {
    self.id() > 0
  }
}

impl Id for String {
  fn id(&self) -> isize {
    123456
  }
}

struct Container<K, V> {
  pair: Option<(K, V)>,
}

impl<K, V> Container<K, V> {
  #[post(ret.is_empty())]
  pub fn new() -> Self {
    Container { pair: None }
  }

  pub fn is_empty(&self) -> bool {
    matches!(self.pair, None)
  }

  #[post(!self.is_empty())]
  pub fn insert(&mut self, k: K, v: V) {
    self.pair = Some((k, v))
  }
}

impl<K: Id, V> Container<K, V> {
  #[post((self.is_empty()).implies(matches!(ret, None)))]
  pub fn get_mut_by_id(&mut self, id: isize) -> Option<&mut V> {
    match &mut self.pair {
      Some((k, v)) if k.id() == id => Some(v),
      _ => None,
    }
  }
}

pub fn main() {
  let mut cont = Container::new();
  let id = 123456;
  let key = "foo".to_string();

  assert!(cont.is_empty());
  assert!(matches!(cont.get_mut_by_id(id), None));

  cont.insert(key.clone(), 0);
  match cont.get_mut_by_id(id) {
    Some(v) => *v = 1000,
    _ => panic!("no value"),
  };

  assert!(matches!(cont, Container { pair: Some((k, 1000))} if k == key))
}
