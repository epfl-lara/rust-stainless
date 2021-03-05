use std::collections::{HashMap, HashSet};

use rustc_hir::def_id::{CrateNum, DefId, DefIndex};
use rustc_hir::lang_items::LangItem as RustLangItem;
use rustc_middle::ty::TyCtxt;

use enum_iterator::IntoEnumIterator;

/// A standard item, either a rust LangItem, or one of the stainless library
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum StdItem {
  LangItem(LangItem),
  CrateItem(CrateItem),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum LangItem {
  FnTrait,
  FnMutTrait,
  FnOnceTrait,
  SizedTrait,
  BeginPanicFn,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum CrateItem {
  BeginPanicFmtFn,
  SetType,
  SetAddFn,
  SetDifferenceFn,
  SetIntersectionFn,
  SetUnionFn,
  SubsetOfFn,
  SetEmptyFn,
  SetSingletonFn,
}

use CrateItem::*;

impl CrateItem {
  /// The path of a crate item, as returned by `tcx.def_path_str`.
  pub fn path(&self) -> &'static str {
    match self {
      BeginPanicFmtFn => "std::rt::begin_panic_fmt",
      SetType => "stainless::Set",
      SetAddFn => "stainless::Set::<T>::add",
      SetDifferenceFn => "stainless::Set::<T>::difference",
      SetIntersectionFn => "stainless::Set::<T>::intersection",
      SetUnionFn => "stainless::Set::<T>::union",
      SubsetOfFn => "stainless::Set::<T>::is_subset_of",
      SetEmptyFn => "stainless::Set::<T>::empty",
      SetSingletonFn => "stainless::Set::<T>::singleton",
    }
  }

  /// An item can (unfortunately) be in a different crate than the first part
  /// of its [path()] suggests. This is due to the aliasing of `alloc` and other
  /// layers under `std`.
  pub fn crate_name(&self) -> &'static str {
    self.path().splitn(2, "::").next().unwrap()
  }
}

lazy_static! {
  static ref CRATE_ITEMS_BY_CRATE: HashMap<&'static str, Vec<CrateItem>> = {
    let mut map = HashMap::new();
    for c in CrateItem::into_enum_iter() {
      map.entry(c.crate_name()).or_insert_with(Vec::new).push(c)
    }
    map
  };
}

impl LangItem {
  fn rust_lang_item(self) -> RustLangItem {
    match self {
      LangItem::FnTrait => RustLangItem::FnTraitLangItem,
      LangItem::FnMutTrait => RustLangItem::FnMutTraitLangItem,
      LangItem::FnOnceTrait => RustLangItem::FnOnceTraitLangItem,
      LangItem::SizedTrait => RustLangItem::SizedTraitLangItem,
      LangItem::BeginPanicFn => RustLangItem::BeginPanicFnLangItem,
    }
  }
}

#[derive(Debug)]
pub(super) struct StdItems {
  def_to_item: HashMap<DefId, StdItem>,
}

impl StdItems {
  pub(super) fn collect(tcx: TyCtxt) -> Self {
    let mut this = Self {
      def_to_item: HashMap::new(),
    };

    let crate_names: HashSet<_> = CRATE_ITEMS_BY_CRATE.keys().collect();
    let crate_nums: HashMap<_, CrateNum> = tcx
      .crates()
      .iter()
      .filter_map(|&cnum| {
        let name = tcx.crate_name(cnum).to_string();
        crate_names.contains(&&name.as_str()).then(|| (name, cnum))
      })
      .collect();

    if !crate_nums.contains_key("stainless") {
      tcx.sess.fatal(
        "Couldn't find stainless library. Make sure it is included using 'extern crate stainless;'",
      )
    }

    // Register rust lang items
    for item in LangItem::into_enum_iter() {
      let def_id = tcx.require_lang_item(item.rust_lang_item(), None);
      this.def_to_item.insert(def_id, StdItem::LangItem(item));
    }

    // Register additional items from the rust standard library
    for (&crate_name, items) in CRATE_ITEMS_BY_CRATE.iter() {
      this.register_items_from_crate(tcx, items, *crate_nums.get(crate_name).unwrap())
    }
    this
  }

  fn make_def_id(cnum: CrateNum, index: usize) -> DefId {
    DefId {
      krate: cnum,
      index: DefIndex::from_usize(index),
    }
  }

  // HACK(gsps): A horrible way to do this, but rustc -- as far as I can tell -- does not expose
  // any API for finding a specific item in, or enumerating all items of a crate.
  fn register_items_from_crate(&mut self, tcx: TyCtxt, items: &[CrateItem], cnum: CrateNum) {
    let mut items: HashMap<&str, _> = items.iter().map(|item| (item.path(), *item)).collect();

    for index in 0.. {
      // We found DefIds for all items, we're done.
      if items.is_empty() {
        break;
      }
      let def_id = Self::make_def_id(cnum, index);
      if let Some(item) = items.remove(tcx.def_path_str(def_id).as_str()) {
        self.def_to_item.insert(def_id, StdItem::CrateItem(item));
      }
    }
  }

  #[inline]
  pub(super) fn def_to_item_opt(&self, def_id: DefId) -> Option<StdItem> {
    self.def_to_item.get(&def_id).copied()
  }

  pub(super) fn is_sized_trait(&self, def_id: DefId) -> bool {
    self
      .def_to_item_opt(def_id)
      .map_or(false, |sti| sti == StdItem::LangItem(LangItem::SizedTrait))
  }

  #[inline]
  pub(super) fn is_fn_like_trait(&self, def_id: DefId) -> bool {
    match self.def_to_item_opt(def_id) {
      Some(StdItem::LangItem(LangItem::FnTrait))
      | Some(StdItem::LangItem(LangItem::FnMutTrait))
      | Some(StdItem::LangItem(LangItem::FnOnceTrait)) => true,
      _ => false,
    }
  }
}
