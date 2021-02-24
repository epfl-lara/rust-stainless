use std::collections::HashMap;

use rustc_hir::def_id::{CrateNum, DefId, DefIndex};
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
  SetType,
  SetAddFn,
  SetDifferenceFn,
  SetIntersectionFn,
  SetUnionFn,
  SubsetOfFn,
  SetEmptyFn,
  SetSingletonFn,
}

const RUST_LANG_ITEMS: &[StdItem] = &[FnTrait, FnMutTrait, FnOnceTrait, SizedTrait, BeginPanicFn];

const STAINLESS_ITEMS: &[StdItem] = &[
  SetType,
  SetAddFn,
  SetDifferenceFn,
  SetIntersectionFn,
  SetUnionFn,
  SubsetOfFn,
  SetEmptyFn,
  SetSingletonFn,
];

use StdItem::*;

impl StdItem {
  pub fn path(self) -> &'static str {
    match self {
      FnTrait => "Fn",
      FnMutTrait => "FnMut",
      FnOnceTrait => "FnOnce",
      SizedTrait => "Sized",
      BeginPanicFn => "begin_panic",
      BeginPanicFmtFn => "begin_panic_fmt",
      SetType => "Set",
      SetAddFn => "add",
      SetDifferenceFn => "difference",
      SetIntersectionFn => "intersection",
      SetUnionFn => "union",
      SubsetOfFn => "is_subset_of",
      SetEmptyFn => "empty",
      SetSingletonFn => "singleton",
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

#[derive(Debug)]
pub(super) struct StdItems {
  def_to_item: HashMap<DefId, StdItem>,
}

const CRATE_NAMES: [&str; 2] = ["std", "stainless"];

impl StdItems {
  pub(super) fn collect(tcx: TyCtxt) -> Self {
    let def_to_item: HashMap<DefId, StdItem> = HashMap::new();

    let mut this = Self { def_to_item };

    let crate_nums: HashMap<String, CrateNum> = tcx
      .crates()
      .iter()
      .filter_map(|&cnum| {
        let name = tcx.crate_name(cnum).to_string();
        CRATE_NAMES.contains(&name.as_str()).then(|| (name, cnum))
      })
      .collect();

    if !crate_nums.contains_key("stainless") {
      tcx.sess.fatal(
        "Couldn't find stainless library. Make sure it is included using 'extern crate stainless;'",
      )
    }

    // Register rust lang items
    this.register_items_from_lang_items(tcx, RUST_LANG_ITEMS);

    // Register additional items from the rust standard library
    this.register_items_from_crate(tcx, &[BeginPanicFmtFn], *crate_nums.get("std").unwrap());
    this.register_items_from_crate(tcx, STAINLESS_ITEMS, *crate_nums.get("stainless").unwrap());
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
      self.def_to_item.insert(def_id, *item);
    }
  }

  // HACK(gsps): A horrible way to do this, but rustc -- as far as I can tell -- does not expose
  // any API for finding a specific item in, or enumerating all items of a crate.
  fn register_items_from_crate(&mut self, tcx: TyCtxt, items: &[StdItem], cnum: CrateNum) {
    let mut items: HashMap<Symbol, StdItem> = items
      .iter()
      .map(|item| (Symbol::intern(item.path()), *item))
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
          dbg!(&tcx.def_path_str(def_id));
          self.def_to_item.insert(def_id, item);
        }
      }
    }
  }

  #[inline]
  pub(super) fn def_to_item_opt(&self, def_id: DefId) -> Option<StdItem> {
    self.def_to_item.get(&def_id).copied()
  }

  #[inline]
  pub(super) fn is_one_of(&self, def_id: DefId, items: &[StdItem]) -> bool {
    self
      .def_to_item_opt(def_id)
      .map_or(false, |sti| items.contains(&sti))
  }
}
