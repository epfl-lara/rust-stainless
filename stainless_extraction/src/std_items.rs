use std::collections::HashMap;

use rustc_hir::def_id::{CrateId, CrateNum, DefId, DefIndex};
use rustc_hir::lang_items::*;
use rustc_middle::ty::TyCtxt;
use rustc_span::symbol::Symbol;

/// A standard item, either a rust LangItem, or one of the stainless library
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum StdItem {
  FnTrait,
  FnMutTrait,
  FnOnceTrait,
  SizedTrait,
  BeginPanicFn,
  BeginPanicFmtFn,
  // Set things,
  SetAddCall,
  SetDifferenceCall,
  SetIntersectionCall,
  SetUnionCall,
  SubsetOfCall,
  FiniteSetCall,
}

const RUST_LANG_ITEMS: &[StdItem] = &[FnTrait, FnMutTrait, FnOnceTrait, SizedTrait, BeginPanicFn];

const STAINLESS_ITEMS: &[StdItem] = &[
  SetAddCall,
  SetDifferenceCall,
  SetIntersectionCall,
  SetUnionCall,
  SubsetOfCall,
  FiniteSetCall,
];

use StdItem::*;

const NUM_STD_ITEMS: usize = 12;

impl StdItem {
  fn index(self) -> usize {
    match self {
      FnTrait => 0,
      FnMutTrait => 1,
      FnOnceTrait => 2,
      SizedTrait => 3,
      BeginPanicFn => 4,
      BeginPanicFmtFn => 5,
      SetAddCall => 6,
      SetDifferenceCall => 7,
      SetIntersectionCall => 8,
      SetUnionCall => 9,
      SubsetOfCall => 10,
      FiniteSetCall => NUM_STD_ITEMS - 1,
    }
  }

  pub fn name(self) -> &'static str {
    match self {
      FnTrait => "Fn",
      FnMutTrait => "FnMut",
      FnOnceTrait => "FnOnce",
      SizedTrait => "Sized",
      BeginPanicFn => "begin_panic",
      BeginPanicFmtFn => "begin_panic_fmt",
      SetAddCall => "add",
      SetDifferenceCall => "difference",
      SetIntersectionCall => "intersection",
      SetUnionCall => "union",
      SubsetOfCall => "is_subset_of",
      FiniteSetCall => "empty",
    }
  }

  pub fn lang_item(self) -> Option<LangItem> {
    match self {
      FnTrait => Some(FnTraitLangItem),
      FnMutTrait => Some(FnMutTraitLangItem),
      FnOnceTrait => Some(FnOnceTraitLangItem),
      SizedTrait => Some(SizedTraitLangItem),
      BeginPanicFn => Some(BeginPanicFnLangItem),
      _ => None,
    }
  }
}

impl From<LangItem> for StdItem {
  fn from(item: LangItem) -> StdItem {
    match item {
      FnTraitLangItem => FnTrait,
      FnMutTraitLangItem => FnMutTrait,
      FnOnceTraitLangItem => FnOnceTrait,
      SizedTraitLangItem => SizedTrait,
      BeginPanicFnLangItem => BeginPanicFn,
      _ => unimplemented!(),
    }
  }
}

#[derive(Debug)]
pub(super) struct StdItems {
  item_to_def: [DefId; NUM_STD_ITEMS],
  def_to_item: HashMap<DefId, StdItem>,
}

impl StdItems {
  pub(super) fn collect(tcx: TyCtxt) -> Self {
    let dummy_def_id = Self::make_def_id(CrateNum::Index(CrateId::from_usize(0)), 0);
    let item_to_def = [dummy_def_id; NUM_STD_ITEMS];
    let def_to_item: HashMap<DefId, StdItem> = HashMap::new();

    let mut this = Self {
      item_to_def,
      def_to_item,
    };

    // Register rust lang items
    this.register_items_from_lang_items(tcx, RUST_LANG_ITEMS);

    // Register additional items from the rust standard library
    let std_crate_num = this.item_to_def(BeginPanicFn).krate;
    this.register_items_from_crate(tcx, &[BeginPanicFmtFn], std_crate_num);

    // Register items from stainless crate
    let stainless_sym = Symbol::intern("stainless");
    let stainless_crate_num = match tcx
      .crates()
      .iter()
      .find(|&&cnum| tcx.crate_name(cnum) == stainless_sym)
      .copied()
    {
      Some(crate_num) => crate_num,
      None => tcx.sess.fatal(
        "Couldn't find stainless library. Make sure it is included using 'extern crate stainless;'",
      ),
    };
    this.register_items_from_crate(tcx, STAINLESS_ITEMS, stainless_crate_num);

    this
  }

  fn make_def_id(cnum: CrateNum, index: usize) -> DefId {
    DefId {
      krate: cnum,
      index: DefIndex::from_usize(index),
    }
  }

  fn register_items_from_lang_items(&mut self, tcx: TyCtxt, items: &[StdItem]) {
    for item in items {
      let def_id = tcx.require_lang_item(item.lang_item().unwrap(), None);
      self.item_to_def[item.index()] = def_id;
      self.def_to_item.insert(def_id, *item);
    }
  }

  // HACK(gsps): A horrible way to do this, but rustc -- as far as I can tell -- does not expose
  // any API for finding a specific item in, or enumerating all items of a crate.
  fn register_items_from_crate(&mut self, tcx: TyCtxt, items: &[StdItem], cnum: CrateNum) {
    let mut items: HashMap<Symbol, StdItem> = items
      .iter()
      .map(|item| (Symbol::intern(item.name()), *item))
      .collect();

    for index in 0.. {
      if items.is_empty() {
        break; // We found DefIds for all items, we're done.
      }
      let def_id = Self::make_def_id(cnum, index);
      if let Some(ref name_sym) = tcx
        .def_path(def_id)
        .data
        .last()
        .map(|data| data.data.as_symbol())
      {
        if let Some(item) = items.remove(name_sym) {
          self.item_to_def[item.index()] = def_id;
          self.def_to_item.insert(def_id, item);
        }
      }
    }
  }

  #[inline]
  pub(super) fn item_to_def<I: Into<StdItem>>(&self, item: I) -> DefId {
    let item = item.into();
    self.item_to_def[item.index()]
  }

  #[inline]
  pub(super) fn def_to_item_opt(&self, def_id: DefId) -> Option<StdItem> {
    self.def_to_item.get(&def_id).copied()
  }

  #[inline]
  pub(super) fn is_one_of(&self, def_id: DefId, items: &[StdItem]) -> bool {
    items.iter().any(|&item| self.item_to_def(item) == def_id)
  }
}
