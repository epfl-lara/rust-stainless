#![allow(non_snake_case)]

#[macro_use]
mod macros;

mod generated;
pub use generated::*;

pub mod pretty;

use std::hash::{Hash, Hasher};

use crate::ser::types::*;
use crate::ser::{BufferSerializer, MarkerId, Serializable, SerializationResult, Serializer};

use bumpalo::Bump;

/// A factory for easily allocating AST nodes in an arena
#[derive(Debug)]
pub struct Factory {
  bump: Bump,
}

impl Factory {
  pub fn new() -> Self {
    Factory { bump: Bump::new() }
  }

  pub fn alloc<T>(&self, v: T) -> &mut T {
    self.bump.alloc(v)
  }
}

/// inox.trees.Symbols
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbols<'a> {
  pub sorts: Map<&'a SymbolIdentifier<'a>, &'a ADTSort<'a>>,
  pub functions: Map<&'a SymbolIdentifier<'a>, &'a FunDef<'a>>,
  pub classes: Map<&'a SymbolIdentifier<'a>, &'a ClassDef<'a>>,
}

impl<'a> Symbols<'a> {
  pub fn new(
    sorts: Seq<&'a ADTSort<'a>>,
    functions: Seq<&'a FunDef<'a>>,
    classes: Seq<&'a ClassDef<'a>>,
  ) -> Self {
    Symbols {
      sorts: sorts.into_iter().map(|s| (s.id, s)).collect(),
      functions: functions.into_iter().map(|s| (s.id, s)).collect(),
      classes: classes.into_iter().map(|s| (s.id, s)).collect(),
    }
  }
}

#[inline]
fn sort_values<K, V: Ord>(map: &Map<K, V>) -> Seq<&V> {
  let mut vs: Vec<_> = map.values().collect();
  vs.sort();
  vs
}

#[inline]
fn hash_seq<K, V: Ord + Hash, H: Hasher>(map: &Map<K, V>, state: &mut H) {
  sort_values(map).iter().for_each(|s| s.hash(state));
}

impl<'a> Hash for Symbols<'a> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    hash_seq(&self.sorts, state);
    hash_seq(&self.functions, state);
    hash_seq(&self.classes, state);
  }
}

impl<'a> Serializable for Symbols<'a> {
  fn serialize<S: Serializer>(&self, s: &mut S) -> SerializationResult {
    let sorts = sort_values(&self.sorts);
    let functions = sort_values(&self.functions);
    let classes = sort_values(&self.classes);

    let mut inner_s = BufferSerializer::new();
    (functions, sorts, classes).serialize(&mut inner_s)?;
    inner_s.to_buffer().serialize(s)
  }
}

// Various trait implementations that are significantly different from the rest

impl<'a> Serializable for ValDef<'a> {
  fn serialize<S: Serializer>(&self, s: &mut S) -> SerializationResult {
    s.write_marker(MarkerId(94))?;
    self.v.serialize(s)
  }
}

impl<'a> Serializable for TypeParameterDef<'a> {
  fn serialize<S: Serializer>(&self, s: &mut S) -> SerializationResult {
    s.write_marker(MarkerId(95))?;
    self.tp.serialize(s)
  }
}

impl Serializable for BVLiteral {
  fn serialize<S: Serializer>(&self, s: &mut S) -> SerializationResult {
    s.write_marker(MarkerId(20))?;
    (self.signed, &self.value, self.size).serialize(s)
  }
}

impl Serializable for Identifier {
  fn serialize<S: Serializer>(&self, s: &mut S) -> SerializationResult {
    s.write_marker(MarkerId(90))?;
    (&self.name, self.globalId, self.id).serialize(s)
  }
}

// TODO: Conditional on stainless build
impl<'a> Serializable for SymbolIdentifier<'a> {
  fn serialize<S: Serializer>(&self, s: &mut S) -> SerializationResult {
    s.write_marker(MarkerId(145))?;
    // NOTE: We deviate from Stainless here in that we reuse the Identifier's globalId
    // as the Symbol's id.
    (
      self.id.globalId,
      self.id.id,
      &self.symbol_path,
      self.id.globalId,
    )
      .serialize(s)
  }
}

// TODO: Conditional on stainless build
impl<'a> Hash for LargeArray<'a> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    let mut elems: Vec<(&Int, &Expr<'a>)> = self.elems.iter().collect();
    elems.sort_by(|&(i1, _), &(i2, _)| i1.cmp(&i2));
    elems.iter().for_each(|elem| elem.hash(state));
    self.default.hash(state);
    self.size.hash(state);
    self.base.hash(state);
  }
}

impl<'l, 'a> From<&'l ValDef<'a>> for &'l Variable<'a> {
  fn from(vd: &'l ValDef<'a>) -> &'l Variable<'a> {
    vd.v
  }
}

// Additional helpers that mirror those in Inox

pub fn Int32Literal(value: Int) -> BVLiteral {
  BVLiteral {
    signed: true,
    value: value.to_bigint().unwrap(),
    size: 32,
  }
}

pub fn Int32Type() -> BVType {
  BVType {
    signed: true,
    size: 32,
  }
}

impl Factory {
  pub fn Int32Literal<'a>(&'a self, value: Int) -> &'a mut BVLiteral {
    self.bump.alloc(Int32Literal(value))
  }

  pub fn Int32Type<'a>(&'a self) -> &'a mut BVType {
    self.bump.alloc(Int32Type())
  }

  pub fn ADTConstructor<'a>(
    &'a self,
    id: &'a SymbolIdentifier<'a>,
    sort: &'a SymbolIdentifier<'a>,
    fields: Seq<&'a ValDef<'a>>,
  ) -> &'a mut ADTConstructor<'a> {
    self.bump.alloc(ADTConstructor { id, sort, fields })
  }

  pub fn MatchCase<'a>(
    &'a self,
    pattern: Pattern<'a>,
    optGuard: Option<Expr<'a>>,
    rhs: Expr<'a>,
  ) -> &'a mut MatchCase<'a> {
    self.bump.alloc(MatchCase {
      pattern,
      optGuard,
      rhs,
    })
  }

  pub fn Identifier<'a>(&'a self, name: String, globalId: Int, id: Int) -> &'a mut Identifier {
    self.bump.alloc(Identifier { name, globalId, id })
  }

  pub fn SymbolIdentifier<'a>(
    &'a self,
    id: &'a Identifier,
    symbol_path: Seq<String>,
  ) -> &'a mut SymbolIdentifier {
    self.bump.alloc(SymbolIdentifier { id, symbol_path })
  }

  /// Extract specs, if any, and wrap them around the body
  pub fn make_and<'a>(&'a self, exprs: Vec<Expr<'a>>) -> Expr<'a> {
    match &exprs[..] {
      [] => self.NoTree(self.Untyped().into()).into(),
      [expr] => *expr,
      _ => self.And(exprs).into(),
    }
  }
}
