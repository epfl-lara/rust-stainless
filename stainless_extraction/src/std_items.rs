use std::collections::{HashMap, HashSet};

use rustc_hir::def::DefKind;
use rustc_hir::def_id::{CrateNum, DefId, DefIndex};
use rustc_hir::lang_items::LangItem as RustLangItem;
use rustc_middle::ty::TyCtxt;

use enum_iterator::IntoEnumIterator;

use super::*;

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
  PanicFn,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum CrateItem {
  BeginPanicFmtFn,
  SetType,
  SetInsertFn,
  SetContainsFn,
  SetDifferenceFn,
  SetIntersectionFn,
  SetUnionFn,
  SetSubsetFn,
  SetNewFn,
  SetSingletonFn,
  BoxNewFn,
  PhantomData,
  ToStringFn,
  StringType,
  PartialEqFn,
  CloneFn,
  CloneTrait,
  ImpliesFn,
  MapType,
  MapNewFn,
  MapIndexFn,
  MapGetFn,
  MapGetOrFn,
  MapContainsKeyFn,
  MapInsertFn,
  MapRemoveFn,
  OptionType,
}

use CrateItem::*;

impl CrateItem {
  /// The path of a crate item, as returned by `tcx.def_path_str`.
  pub fn path(&self) -> &'static str {
    match self {
      BeginPanicFmtFn => "std::rt::begin_panic_fmt",
      SetType => "stainless::Set",
      SetInsertFn => "stainless::Set::<T>::insert",
      SetContainsFn => "stainless::Set::<T>::contains",
      SetDifferenceFn => "stainless::Set::<T>::difference",
      SetIntersectionFn => "stainless::Set::<T>::intersection",
      SetUnionFn => "stainless::Set::<T>::union",
      SetSubsetFn => "stainless::Set::<T>::is_subset",
      SetNewFn => "stainless::Set::<T>::new",
      SetSingletonFn => "stainless::Set::<T>::singleton",
      BoxNewFn => "std::boxed::Box::<T>::new",
      PhantomData => "std::marker::PhantomData",
      ToStringFn => "std::string::ToString::to_string",
      StringType => "std::string::String",
      PartialEqFn => "std::cmp::PartialEq::eq",
      CloneFn => "std::clone::Clone::clone",
      CloneTrait => "std::clone::Clone",
      ImpliesFn => "stainless::Implies::implies",
      MapType => "stainless::Map",
      MapNewFn => "stainless::Map::<K, V>::new",
      MapIndexFn => "stainless::Map::<K, V>::index",
      MapGetFn => "stainless::Map::<K, V>::get",
      MapGetOrFn => "stainless::Map::<K, V>::get_or",
      MapContainsKeyFn => "stainless::Map::<K, V>::contains_key",
      MapInsertFn => "stainless::Map::<K, V>::insert",
      MapRemoveFn => "stainless::Map::<K, V>::remove",
      OptionType => "std::option::Option",
    }
  }

  /// An item can (unfortunately) be in a different crate than the first part
  /// of its [path()] suggests. This is due to the aliasing of `alloc` and other
  /// layers under `std`.
  pub fn crate_name(&self) -> &'static str {
    match self {
      BoxNewFn | ToStringFn | StringType => "alloc",
      PhantomData | PartialEqFn | CloneFn | CloneTrait | OptionType => "core",
      _ => self.path().splitn(2, "::").next().unwrap(),
    }
  }

  pub fn def_kind(&self) -> DefKind {
    match self {
      BeginPanicFmtFn => DefKind::Fn,
      SetType | MapType | StringType | PhantomData => DefKind::Struct,
      CloneTrait => DefKind::Trait,
      OptionType => DefKind::Enum,
      _ => DefKind::AssocFn,
    }
  }

  pub fn is_set_related(&self) -> bool {
    matches!(
      self,
      SetType
        | SetNewFn
        | SetSingletonFn
        | SetInsertFn
        | SetContainsFn
        | SetDifferenceFn
        | SetIntersectionFn
        | SetUnionFn
        | SetSubsetFn
    )
  }

  pub fn is_map_related(&self) -> bool {
    matches!(
      self,
      MapType
        | MapNewFn
        | MapIndexFn
        | MapContainsKeyFn
        | MapGetFn
        | MapGetOrFn
        | MapInsertFn
        | MapRemoveFn
    )
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
      LangItem::FnTrait => RustLangItem::Fn,
      LangItem::FnMutTrait => RustLangItem::FnMut,
      LangItem::FnOnceTrait => RustLangItem::FnOnce,
      LangItem::SizedTrait => RustLangItem::Sized,
      LangItem::BeginPanicFn => RustLangItem::BeginPanic,
      LangItem::PanicFn => RustLangItem::Panic,
    }
  }
}

#[derive(Debug, Default)]
pub(super) struct StdItems {
  def_to_item: HashMap<DefId, StdItem>,
  item_to_def: HashMap<StdItem, DefId>,
}

impl StdItems {
  pub(super) fn collect(tcx: TyCtxt) -> Self {
    let mut this = Self::default();

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
      this.item_to_def.insert(StdItem::LangItem(item), def_id);
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
      let path = tcx.def_path_str(def_id);

      // FIXME: we have to query the map twice, because for some def_ids the
      //   `tcx.def_kind` may panic. Once update to nightly > 2021, one can use
      //   `tcx.opt_def_kind` to eliminate the problem.
      if let Some(item) = items.get(path.as_str()) {
        if tcx.def_kind(def_id) == item.def_kind() {
          if let Some(item) = items.remove(path.as_str()) {
            self.def_to_item.insert(def_id, StdItem::CrateItem(item));
            self.item_to_def.insert(StdItem::CrateItem(item), def_id);
          }
        }
      }
    }
  }

  #[inline]
  pub(super) fn def_to_item_opt(&self, def_id: DefId) -> Option<StdItem> {
    self.def_to_item.get(&def_id).copied()
  }

  #[inline]
  pub(super) fn item_to_def(&self, item: StdItem) -> DefId {
    // By construction, we know that all items are in the map.
    self.item_to_def.get(&item).copied().unwrap()
  }

  pub(super) fn is_sized_trait(&self, def_id: DefId) -> bool {
    self
      .def_to_item_opt(def_id)
      .map_or(false, |sti| sti == StdItem::LangItem(LangItem::SizedTrait))
  }

  #[inline]
  pub(super) fn is_fn_like_trait(&self, def_id: DefId) -> bool {
    matches!(
      self.def_to_item_opt(def_id),
      Some(StdItem::LangItem(LangItem::FnTrait))
        | Some(StdItem::LangItem(LangItem::FnMutTrait))
        | Some(StdItem::LangItem(LangItem::FnOnceTrait))
    )
  }
}
