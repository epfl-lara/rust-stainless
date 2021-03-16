extern crate stainless;
use stainless::*;

trait Clone {
  fn clone(&self) -> Self;
}

pub enum Option<T> {
  Some(T),
  None,
}
impl<T: Clone> Clone for Option<T> {
  #[measure(self)]
  fn clone(&self) -> Self {
    match self {
      Option::Some(v) => Option::Some(v.clone()),
      _ => Option::None,
    }
  }
}

pub enum Wood {
  Oak,
  Cedar,
}
impl Clone for Wood {
  fn clone(&self) -> Self {
    match self {
      Wood::Oak => Wood::Oak,
      Wood::Cedar => Wood::Cedar,
    }
  }
}

pub enum Material {
  Metal,
  Wooden(Wood),
}
impl Clone for Material {
  fn clone(&self) -> Self {
    match self {
      Material::Metal => Material::Metal,
      Material::Wooden(o) => Material::Wooden((*o).clone()),
    }
  }
}

pub struct Table {
  material: Option<Material>,
}
impl Clone for Table {
  fn clone(&self) -> Self {
    Table {
      material: self.material.clone(),
    }
  }
}
