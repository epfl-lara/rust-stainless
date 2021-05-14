use super::*;

use rustc_hir::def_id::DefId;
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, def, AssocItemKind, Defaultness, ItemKind, Path};
use rustc_hir_pretty as pretty;
use rustc_middle::ty::{AssocKind, List, TraitRef};
use rustc_span::DUMMY_SP;

use stainless_data::ast as st;

use crate::flags::Flag;
use crate::fns::{FnItem, FnSignature};

use std::iter;

/// Top-level extraction
impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  /// Entrypoint into Extractor
  pub fn process_crate(&mut self, _crate_name: String) {
    struct ItemVisitor<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut BaseExtractor<'l, 'tcx>,
      functions: Vec<FnItem<'l>>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        let def_id = self.xtor.hir_to_def_id(item.hir_id());
        let def_path_str = self.xtor.tcx.def_path_str(def_id);

        match &item.kind {
          // Ignore extern crate statements for allowed crates.
          ItemKind::ExternCrate(_)
            if matches!(item.ident.name.to_string().as_str(), "std" | "stainless") => {}

          // Ignore module declarations
          ItemKind::Mod(..) => {}

          ItemKind::Use(path, _) => self.xtor.extract_use(path, item.span),

          // Store top-level functions
          ItemKind::Fn(..) => {
            eprintln!("  - Fun {}", def_path_str);
            let fn_item = self.xtor.create_fn_item(def_id, false);
            self.functions.push(fn_item);
          }

          // Store ADTs
          ItemKind::Enum(..) | ItemKind::Struct(..) => {
            eprintln!("  - ADT {}", def_path_str);
            self.xtor.get_or_extract_adt(def_id);
          }

          // Store functions of impl blocks and their specs
          ItemKind::Impl(hir::Impl { items, .. }) => {
            // if the impl implements a trait, then we need to extract it as a class/object.
            let trait_ref = self.xtor.tcx.impl_trait_ref(def_id);

            // Some trait implementations are ignored/erased.
            if !self.xtor.should_ignore_trait_impl(trait_ref) {
              let class_def =
                trait_ref.map(|t| self.xtor.extract_class(def_id, Some(t), item.span));

              self.extract_class_item(
                items.iter().map(|i| (i.id.hir_id(), i.kind, i.defaultness)),
                class_def,
              )
            }
          }

          // Extract the trait as an abstract class. Laws and other concrete
          // functions can be extracted like any normal function. Abstract
          // functions are marked and will be treated accordingly.
          ItemKind::Trait(_, _, _, _, items) => {
            let cd = self.xtor.extract_class(def_id, None, item.span);
            self.extract_class_item(
              items.iter().map(|i| (i.id.hir_id(), i.kind, i.defaultness)),
              Some(cd),
            )
          }

          _ => {
            self
              .xtor
              .unsupported(item.span, format!("Other kind of item {:?}", item.kind));
          }
        }
      }

      /// Ignore fn items in because they are already treated when the entire
      /// impl/trait block is extracted.
      fn visit_trait_item(&mut self, trait_item: &'tcx hir::TraitItem<'tcx>) {
        match trait_item.kind {
          hir::TraitItemKind::Fn(..) => {}
          _ => self
            .xtor
            .unsupported(trait_item.span, "Trait item other than function"),
        }
      }

      /// Ignore fn items in because they are already treated when the entire
      /// impl/trait block is extracted.
      fn visit_impl_item(&mut self, impl_item: &'tcx hir::ImplItem<'tcx>) {
        match impl_item.kind {
          hir::ImplItemKind::Fn(..) => {}
          _ => self
            .xtor
            .unsupported(impl_item.span, "Impl item other than function"),
        }
      }

      fn visit_foreign_item(&mut self, f: &'tcx hir::ForeignItem<'tcx>) {
        self.xtor.unsupported(f.span, "Foreign item")
      }
    }

    impl<'l> ItemVisitor<'_, 'l, '_> {
      fn extract_class_item<I>(&mut self, items: I, class_def: Option<&'l st::ClassDef<'l>>)
      where
        I: Iterator<Item = (HirId, AssocItemKind, Defaultness)>,
      {
        let fns = items
          .filter_map(|(hir_id, kind, defaultness)| match kind {
            AssocItemKind::Fn { .. } => {
              let fn_id = self.xtor.hir_to_def_id(hir_id);
              Some(self.xtor.create_fn_item(fn_id, !defaultness.has_value()))
            }
            // ignore consts and type aliases in impl blocks
            _ => None,
          })
          .collect::<Vec<_>>();

        // Add the class with references to its methods to the extraction
        if let Some(cd) = class_def {
          self.xtor.add_class(cd, fns.iter().map(|fi| fi.fd_id))
        }
        // Add the functions to the visitor for further extraction
        self.functions.extend(fns);
      }
    }

    let krate = self.tcx.hir().krate();

    // Discover items in the local crate
    let mut visitor = ItemVisitor {
      xtor: self,
      functions: vec![],
    };
    eprintln!("[ Discovering local definitions ]");
    krate.visit_all_item_likes(&mut visitor);
    eprintln!();

    let (abstract_fns, fns): (Vec<FnItem>, Vec<FnItem>) =
      visitor.functions.iter().partition(|f| f.is_abstract);

    // Extract abstract functions
    for fun in abstract_fns {
      self.get_or_extract_abstract_fn(fun.def_id);
    }

    // Extract concrete local functions (this includes laws)
    for fn_item in fns {
      self.get_or_extract_local_fn(&fn_item);
    }

    // Extract external items as stubs
    self
      .with_extraction(|xt| {
        // Find all those functions that have been referenced, but not yet extracted
        xt.function_refs
          .iter()
          // take only the non-local ones
          .filter(|def_id| !def_id.is_local())
          .copied()
          .collect::<Vec<_>>()
      })
      .iter()
      // And extract them as external functions
      .for_each(|&def_id| {
        self.get_or_extract_extern_fn(def_id);
      })
  }

  fn create_fn_item(&mut self, def_id: DefId, is_abstract: bool) -> FnItem<'l> {
    FnItem::new(def_id, self.get_or_extract_fn_ref(def_id), is_abstract)
  }

  /// We ignore i.e. erase some trait implementations, like Clone, on purpose
  /// because we also erase calls to `Clone::clone`. Therefore, the
  /// implementation is never called in the Stainless program and it needs to be
  /// erased as well. Clone erasure is safe under the assumptions stated here:
  /// https://github.com/epfl-lara/rust-stainless/issues/136
  ///
  /// FIXME: the soundness checks are not implemented yet.
  fn should_ignore_trait_impl(&self, trait_ref: Option<TraitRef<'tcx>>) -> bool {
    matches!(
      trait_ref.and_then(|t| self.std_items.def_to_item_opt(t.def_id)),
      Some(StdItem::CrateItem(CrateItem::CloneTrait))
    )
  }

  /// Extract a function reference (regardless of whether it is local or external)
  // TODO: Extract flags on functions and parameters
  pub(super) fn get_or_extract_fn_ref(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    self.get_id_from_def(def_id).unwrap_or_else(|| {
      self.add_function_ref(def_id);

      // Check whether this function implements some method of a trait.
      // More specifically, if the function has a name
      let overridden_def_id: Option<DefId> =
        self.tcx.opt_item_name(def_id).and_then(|fn_item_name| {
          self
            .tcx
            // and it is a method of an impl block
            .impl_of_method(def_id)
            // and that impl block implements a trait
            .and_then(|impl_id| self.tcx.trait_id_of_impl(impl_id))
            // and that trait has a method of the same name,
            .and_then(|trait_id| {
              self.tcx.associated_items(trait_id).find_by_name_and_kind(
                self.tcx,
                fn_item_name,
                AssocKind::Fn,
                trait_id,
              )
            })
            // then we want to use that method's symbol path.
            .map(|item| item.def_id)
        });

      match overridden_def_id {
        // To signal overriding to stainless, the overriding method needs to have
        // the symbol path of the abstract (trait's) method.
        Some(odi) => self.register_def_with_path(def_id, self.symbol_path_from_def_id(odi)),
        _ => self.register_def(def_id),
      }
    })
  }

  fn get_fn(&mut self, def_id: DefId) -> Option<&'l st::FunDef<'l>> {
    self.with_extraction(|xt| {
      xt.mapping
        .did_to_stid
        .get(&def_id)
        .and_then(|stid| xt.functions.get(stid).copied())
    })
  }

  pub fn get_or_extract_extern_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    self.get_fn(def_id).unwrap_or_else(|| {
      let fd = self.extract_extern_fn(def_id);
      self.add_function(fd);
      fd
    })
  }

  /// Extract an external function
  pub(super) fn extract_extern_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    assert!(
      !def_id.is_local(),
      "Expected non-local def id, got: {:?}",
      def_id
    );
    let FnSignature {
      id,
      tparams,
      params,
      return_tpe,
    } = self.extract_fn_signature(def_id);

    let f = self.factory();
    let empty_body = f.NoTree(return_tpe).into();
    f.FunDef(
      id,
      tparams,
      params,
      return_tpe,
      empty_body,
      vec![f.Extern().into()],
    )
  }

  pub fn get_or_extract_abstract_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    self.get_fn(def_id).unwrap_or_else(|| {
      let fd = self.extract_abstract_fn(def_id);
      self.add_function(fd);
      fd
    })
  }

  fn extract_abstract_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    let FnSignature {
      id,
      tparams,
      params,
      return_tpe,
    } = self.extract_fn_signature(def_id);
    let class_def = self.get_class_of_method(id);

    let f = self.factory();
    let empty_body = f.NoTree(return_tpe).into();
    f.FunDef(
      id,
      self.filter_class_tparams(tparams, class_def),
      params,
      return_tpe,
      empty_body,
      class_def
        .iter()
        .map(|cd| f.IsMethodOf(cd.id).into())
        .chain(iter::once(f.IsAbstract().into()))
        .collect(),
    )
  }

  fn extract_fn_signature(&mut self, def_id: DefId) -> FnSignature<'l> {
    let f = self.factory();

    // Extract the function signature
    let Generics { tparams, txtcx, .. } = self.get_or_extract_generics(def_id);
    let fn_sig = self.ty_fn_sig(def_id);
    let params: Params<'l> = fn_sig
      .inputs()
      .iter()
      .enumerate()
      .map(|(i, ty)| {
        let id = self.fresh_id(format!("param{}", i));
        let tpe = self.extract_ty(ty, &txtcx, DUMMY_SP);
        let var = f.Variable(id, tpe, vec![]);
        &*f.ValDef(var)
      })
      .collect();

    FnSignature {
      id: self.get_or_extract_fn_ref(def_id),
      tparams,
      params,
      return_tpe: self.extract_ty(fn_sig.output(), &txtcx, DUMMY_SP),
    }
  }

  pub fn get_or_extract_local_fn(&mut self, fn_item: &FnItem<'l>) -> &'l st::FunDef<'l> {
    self.get_fn(fn_item.def_id).unwrap_or_else(|| {
      let fd = self.extract_local_fn(fn_item);
      self.add_function(fd);
      fd
    })
  }

  /// Extract a local function
  pub(super) fn extract_local_fn(&mut self, fn_item: &FnItem<'l>) -> &'l st::FunDef<'l> {
    let f = self.factory();

    assert!(fn_item.def_id.is_local());
    let hir_id = self.ldef_to_hir_id(fn_item.def_id.expect_local());
    let class_def = self.get_class_of_method(fn_item.fd_id);

    // Extract flags
    let (carrier_flags, mut flags_by_symbol) = self.extract_flags(hir_id);
    let mut flags = carrier_flags.to_stainless(f);

    // As long as we only have local mutation, every function viewed from the
    // outside is pure.
    flags.push(f.IsPure().into());

    // Add flag specifying that this function is a method of its class (if there's a class)
    flags.extend(
      class_def
        .iter()
        .map(|cd| -> st::Flag<'l> { f.IsMethodOf(cd.id).into() }),
    );

    // Extract the function itself
    let Generics {
      tparams,
      txtcx,
      trait_bounds,
    } = self.get_or_extract_generics(fn_item.def_id);

    // If this function is not on a class *but* has trait bounds, we need to add
    // these as evidence parameters.
    let ev_params = if class_def.is_none() {
      self.evidence_params(trait_bounds)
    } else {
      vec![]
    };

    let (params, return_tpe, body_expr): (Params<'l>, st::Type<'l>, st::Expr<'l>) = self
      .enter_body(hir_id, txtcx, class_def, |bxtor| {
        // Register parameters and local bindings in the DefContext
        bxtor.populate_def_context(&mut flags_by_symbol, &ev_params);

        let body_expr = bxtor.extract_body_expr(fn_item.def_id.expect_local());
        let body_expr = bxtor.wrap_body_let_vars(body_expr);

        (bxtor.dcx.params().to_vec(), bxtor.return_tpe(), body_expr)
      });

    self.report_unused_flags(hir_id, &flags_by_symbol);

    // Wrap it all up in a Stainless function
    f.FunDef(
      fn_item.fd_id,
      self.filter_class_tparams(tparams, class_def),
      params,
      return_tpe,
      body_expr,
      flags,
    )
  }

  /// Filter out the tparams of the class and the class (as tparam) itself
  fn filter_class_tparams(
    &self,
    tparams: Vec<&'l st::TypeParameterDef<'l>>,
    class_def: Option<&'l st::ClassDef<'l>>,
  ) -> Vec<&'l st::TypeParameterDef<'l>> {
    match class_def {
      None => tparams,
      Some(cd) => tparams
        .into_iter()
        .filter(|&tpd| tpd.tp.id != cd.id && !cd.tparams.contains(&tpd))
        .collect(),
    }
  }

  /// Extract a use statement. Mostly they are ignored but the user gets a
  /// warning if they import something Stainless may not know.
  fn extract_use(&mut self, path: &Path, span: Span) {
    match path {
      // Ignore crate-local use statements
      Path {
        res: def::Res::Def(_, def_id),
        ..
      } if def_id.is_local() => {}

      // Extract the 'use PhantomData' statement as the PhantomData ADT definition.
      Path {
        res: def::Res::Def(def::DefKind::Struct, def_id),
        ..
      } if matches!(
        self.std_items.def_to_item_opt(*def_id),
        Some(StdItem::CrateItem(CrateItem::PhantomData))
      ) =>
      {
        self.get_or_extract_adt(*def_id);
      }

      // If the use is not from stainless or some special std, then warn the
      // user about that import.
      _ => {
        let path_str = pretty_path(path);
        if !path_str.contains("std::prelude")
          && !path_str.starts_with("std::marker::PhantomData")
          && !path_str.starts_with("std::hash::Hash")
          && !path_str.starts_with("stainless")
        {
          self.tcx.sess.span_warn(
            span,
            "Unknown use statement. Stainless likely cannot deal with the imported items.",
          )
        }
      }
    }
  }

  /// Extract an ADT (regardless of whether it is local or external) or return it from the map of
  /// already extracted ADTs.
  /// Caution: if you only need the ADT's id, you should call [get_or_register_def] because there are
  /// cases where the ID is already in the cache but not the entire ADTSort. That may lead to
  /// infinite recursion.
  pub(super) fn get_or_extract_adt(&mut self, def_id: DefId) -> &'l st::ADTSort<'l> {
    self
      .get_id_from_def(def_id)
      .and_then(|id| self.get_adt(id))
      .unwrap_or_else(|| {
        let adt = self.extract_adt(def_id);
        self.add_adt(adt)
      })
  }

  fn extract_adt(&mut self, def_id: DefId) -> &'l st::ADTSort<'l> {
    let f = self.factory();
    let adt_id = self.get_or_register_def(def_id);
    let adt_def = self.tcx.adt_def(def_id);

    // Extract flags for local def ids.
    let (flags, mut flags_by_symbol, hir_id_opt) = self
      .def_to_hir_id(def_id)
      .map(|hir_id| {
        let (carrier_flags, by_symbol) = self.extract_flags(hir_id);
        (carrier_flags.to_stainless(f), by_symbol, Some(hir_id))
      })
      // TODO: Extract external (non-local) flags. Tracked here:
      //   https://github.com/epfl-lara/rust-stainless/issues/36
      //   Currently, we just return empty flags for non-local ADTs.
      .unwrap_or_default();

    // Extract generics
    let Generics { tparams, txtcx, .. } = self.get_or_extract_generics(def_id);

    // Extract constructors
    let constructors = adt_def
      .variants
      .iter()
      .map(|variant| {
        let cons_id = self.get_or_register_def(variant.def_id);
        let fields = variant
          .fields
          .iter()
          .map(|field| {
            let field_id = self.get_or_register_def(field.did);
            let substs = List::identity_for_item(self.tcx, def_id);
            let field_ty = field.ty(self.tcx, substs);
            let field_ty = self.extract_ty(field_ty, &txtcx, field.ident.span);

            let mut flags = flags_by_symbol
              .remove(&field.ident.name)
              .map(|flags| flags.to_stainless(f))
              .unwrap_or_default();
            flags.push(f.IsVar().into());
            let field = f.Variable(field_id, field_ty, flags);
            &*f.ValDef(field)
          })
          .collect();
        &*f.ADTConstructor(cons_id, adt_id, fields)
      })
      .collect();

    if let Some(hir_id) = hir_id_opt {
      self.report_unused_flags(hir_id, &flags_by_symbol)
    }

    f.ADTSort(adt_id, tparams, constructors, flags)
  }
}

fn pretty_path(path: &Path<'_>) -> String {
  pretty::to_string(pretty::NO_ANN, |s| s.print_path(path, false))
}
