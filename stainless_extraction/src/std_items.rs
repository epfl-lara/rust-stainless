use std::collections::{HashMap, HashSet};

use rustc_hir::def_id::{CrateNum, DefId, DefIndex};
use rustc_hir::lang_items::LangItem;
use rustc_middle::ty::TyCtxt;

use enum_iterator::IntoEnumIterator;

/// A standard item, either a rust LangItem, or one of the stainless library
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum StdItem {
  Fn(StdItemFn),
  Type(StdItemType),
  Trait(StdItemTrait),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum StdItemFn {
  BeginPanic,
  BeginPanicFmt,
  SetAdd,
  SetDifference,
  SetIntersection,
  SetUnion,
  SubsetOf,
  SetEmpty,
  SetSingleton,
  BoxNew,
}
use StdItemFn::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum StdItemType {
  PhantomData,
  Set,
}
use StdItemType::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, IntoEnumIterator)]
pub enum StdItemTrait {
  Fn,
  FnMut,
  FnOnce,
  Sized,
}
use StdItemTrait::*;

impl StdItem {
  fn origin(&self) -> StdItemOrigin {
    match self {
      StdItem::Trait(Fn) => Lang(LangItem::FnTraitLangItem),
      StdItem::Trait(FnMut) => Lang(LangItem::FnMutTraitLangItem),
      StdItem::Trait(FnOnce) => Lang(LangItem::FnOnceTraitLangItem),
      StdItem::Trait(Sized) => Lang(LangItem::SizedTraitLangItem),
      StdItem::Fn(BeginPanic) => Lang(LangItem::BeginPanicFnLangItem),

      StdItem::Fn(BeginPanicFmt) => Crate(CratePath::new("std::rt::begin_panic_fmt")),
      StdItem::Fn(BoxNew) => Crate(CratePath::with_crate("alloc", "std::boxed::Box::<T>::new")),
      StdItem::Type(PhantomData) => {
        Crate(CratePath::with_crate("core", "std::marker::PhantomData"))
      }

      StdItem::Type(Set) => Crate(CratePath::new("stainless::Set")),
      StdItem::Fn(SetAdd) => Crate(CratePath::new("stainless::Set::<T>::add")),
      StdItem::Fn(SetDifference) => Crate(CratePath::new("stainless::Set::<T>::difference")),
      StdItem::Fn(SetIntersection) => Crate(CratePath::new("stainless::Set::<T>::intersection")),
      StdItem::Fn(SetUnion) => Crate(CratePath::new("stainless::Set::<T>::union")),
      StdItem::Fn(SubsetOf) => Crate(CratePath::new("stainless::Set::<T>::is_subset_of")),
      StdItem::Fn(SetEmpty) => Crate(CratePath::new("stainless::Set::<T>::empty")),
      StdItem::Fn(SetSingleton) => Crate(CratePath::new("stainless::Set::<T>::singleton")),
    }
  }

  pub fn into_enum_iter() -> impl Iterator<Item = Self> {
    StdItemFn::into_enum_iter()
      .map(|f| StdItem::Fn(f))
      .chain(StdItemType::into_enum_iter().map(|t| StdItem::Type(t)))
      .chain(StdItemTrait::into_enum_iter().map(|t| StdItem::Trait(t)))
  }
}

enum StdItemOrigin {
  /// An item corresponding to a Rust internal `rustc_hir::lang_items::LangItem`
  Lang(LangItem),
  Crate(CratePath),
}
use StdItemOrigin::*;

struct CratePath {
  /// The path of an item, as returned by `tcx.def_path_str`.
  crate_name: &'static str,

  /// An item can (unfortunately) be in a different crate than the first part
  /// of its [path()] suggests. This is due to the aliasing of `alloc` and other
  /// layers under `std`.
  path: &'static str,
}

impl CratePath {
  fn new(path: &'static str) -> Self {
    CratePath {
      path,
      crate_name: path.splitn(2, "::").next().unwrap(),
    }
  }

  fn with_crate(crate_name: &'static str, path: &'static str) -> Self {
    CratePath { crate_name, path }
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

    let mut items_by_crate_name = HashMap::new();
    for item in StdItem::into_enum_iter() {
      match item.origin() {
        // Register rust lang items
        Lang(rust_lang_item) => {
          let def_id = tcx.require_lang_item(rust_lang_item, None);
          this.def_to_item.insert(def_id, item);
        }
        // Put the crate items in a map by crate_name
        Crate(cp) => items_by_crate_name
          .entry(cp.crate_name)
          .or_insert_with(Vec::new)
          .push((cp.path, item)),
      }
    }

    let crate_names: HashSet<_> = items_by_crate_name.keys().collect();
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

    // Register additional items from the rust standard library
    for (&crate_name, items) in items_by_crate_name.iter() {
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
  fn register_items_from_crate(&mut self, tcx: TyCtxt, items: &[(&str, StdItem)], cnum: CrateNum) {
    let mut items: HashMap<&str, _> = items.iter().copied().collect();

    for index in 0.. {
      // We found DefIds for all items, we're done.
      if items.is_empty() {
        break;
      }
      let def_id = Self::make_def_id(cnum, index);
      if let Some(item) = items.remove(tcx.def_path_str(def_id).as_str()) {
        self.def_to_item.insert(def_id, item);
      }
    }
  }

  #[inline]
  fn get_item(&self, def_id: DefId) -> Option<StdItem> {
    self.def_to_item.get(&def_id).copied()
  }

  #[inline]
  pub(super) fn get_fn_item(&self, def_id: DefId) -> Option<StdItemFn> {
    self.get_item(def_id).and_then(|i| match i {
      StdItem::Fn(f) => Some(f),
      _ => None,
    })
  }

  #[inline]
  pub(super) fn get_ty_item(&self, def_id: DefId) -> Option<StdItemType> {
    self.get_item(def_id).and_then(|i| match i {
      StdItem::Type(t) => Some(t),
      _ => None,
    })
  }

  #[inline]
  pub(super) fn is_sized_trait(&self, def_id: DefId) -> bool {
    self
      .get_item(def_id)
      .map_or(false, |sti| sti == StdItem::Trait(Sized))
  }

  #[inline]
  pub(super) fn is_fn_like_trait(&self, def_id: DefId) -> bool {
    match self.get_item(def_id) {
      Some(StdItem::Trait(Fn)) | Some(StdItem::Trait(FnMut)) | Some(StdItem::Trait(FnOnce)) => true,
      _ => false,
    }
  }
}
