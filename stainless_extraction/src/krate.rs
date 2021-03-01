use super::*;

use rustc_hir::def_id::DefId;
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, AssocItemKind, Defaultness, ItemKind};
use rustc_hir_pretty as pretty;
use rustc_middle::ty::{AssocKind, List};
use rustc_span::DUMMY_SP;

use stainless_data::ast as st;
use stainless_data::ast::{SymbolIdentifier, TypeParameterDef};

use crate::fns::FnItem;
use std::iter;

/// Top-level extraction

impl<'l, 'tcx> BaseExtractor<'l, 'tcx> {
  /// Entrypoint into Extractor
  pub fn process_crate(&mut self, _crate_name: String) {
    fn pretty_path(path: &hir::Path<'_>) -> String {
      pretty::to_string(pretty::NO_ANN, |s| s.print_path(path, false))
    }

    // TODO: Ignore certain boilerplate/compiler-generated items
    fn should_ignore<'tcx>(item: &'tcx hir::Item<'tcx>) -> bool {
      match item.kind {
        ItemKind::ExternCrate(_) => {
          let name = item.ident.name.to_string();
          name == "std" || name == "stainless"
        }
        ItemKind::Use(ref path, _) => {
          let path_str = pretty_path(path);
          path_str.contains("std::prelude") || path_str.starts_with("stainless")
        }
        // TODO: Quick fix to filter our synthetic functions
        // ItemKind::Fn(..) if !item.attrs.is_empty() => true,
        _ => false,
      }
    }

    struct ItemVisitor<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut BaseExtractor<'l, 'tcx>,
      functions: Vec<FnItem<'l>>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        let def_id = self.xtor.tcx.hir().local_def_id(item.hir_id).to_def_id();
        let def_path_str = self.xtor.tcx.def_path_str(def_id);

        match &item.kind {
          // Ignore use and external crates, see #should_ignore.
          _ if should_ignore(item) => {}

          // Store top-level functions
          ItemKind::Fn(..) => {
            eprintln!("  - Fun {}", def_path_str);
            let fn_item = self.xtor.create_fn_item(def_id, false);
            self.functions.push(fn_item);
          }

          // Store ADTs
          ItemKind::Enum(..) | ItemKind::Struct(..) => {
            eprintln!("  - ADT {}", def_path_str);
            let sort = self.xtor.extract_adt(def_id);
            self.xtor.add_adt(sort.id, sort);
          }

          // Store functions of impl blocks and their specs
          ItemKind::Impl { items, .. } => {
            // if the impl implements a trait, then we need to extract it as a class/object.
            let class_def: Option<&'l st::ClassDef<'l>> = self
              .xtor
              .tcx
              .impl_trait_ref(def_id)
              .map(|trait_ref| self.xtor.extract_class(def_id, Some(trait_ref), item.span));

            self.extract_class_item(
              items.iter().map(|i| (i.id.hir_id, i.kind, i.defaultness)),
              class_def,
            )
          }

          // Extract the trait as an abstract class, the laws as normal
          // functions and the abstract functions as empty functions.
          ItemKind::Trait(_, _, _, _, items) => {
            let cd = self.xtor.extract_class(def_id, None, item.span);

            // Laws and other concrete functions can be extracted like any
            // normal function. Abstract functions are marked and will be
            // treated accordingly.
            self.extract_class_item(
              items.iter().map(|i| (i.id.hir_id, i.kind, i.defaultness)),
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
    }

    impl<'l> ItemVisitor<'_, 'l, '_> {
      fn extract_class_item<I>(&mut self, items: I, class_def: Option<&'l st::ClassDef<'l>>)
      where
        I: Iterator<Item = (HirId, AssocItemKind, Defaultness)>,
      {
        let fns = items
          .filter_map(|(hir_id, kind, defaultness)| match kind {
            AssocItemKind::Fn { .. } => {
              let fn_id = self.xtor.tcx.hir().local_def_id(hir_id).to_def_id();
              Some(self.xtor.create_fn_item(fn_id, !defaultness.has_value()))
            }
            // ignore consts and type aliases in impl blocks
            _ => None,
          })
          .collect::<Vec<_>>();

        // Add the class with references to its methods to the extraction
        class_def.map(|cd| {
          self
            .xtor
            .add_class(cd, fns.iter().map(|fi| fi.fd_id).collect())
        });
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
      let fd = self.extract_abstract_fn(fun.def_id);
      self.add_function(fd);
    }

    // Extract concrete local functions (this includes laws)
    for fn_item in fns {
      let fd = self.extract_local_fn(&fn_item);
      self.add_function(fd);
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
        let fd = self.extract_extern_fn(def_id);
        self.add_function(fd);
      })
  }

  fn create_fn_item(&mut self, def_id: DefId, is_abstract: bool) -> FnItem<'l> {
    FnItem::new(def_id, self.extract_fn_ref(def_id), is_abstract)
  }

  /// Extract a function reference (regardless of whether it is local or external)
  // TODO: Extract flags on functions and parameters
  pub(super) fn extract_fn_ref(&mut self, def_id: DefId) -> StainlessSymId<'l> {
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

  /// Extract an external function
  pub(super) fn extract_extern_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    assert!(
      !def_id.is_local(),
      "Expected non-local def id, got: {:?}",
      def_id
    );
    let (id, tparams, params, rtp) = self.extract_fn_signature(def_id);

    let f = self.factory();
    let empty_body = f.NoTree(rtp).into();
    f.FunDef(
      id,
      tparams,
      params,
      rtp,
      empty_body,
      vec![f.Extern().into()],
    )
  }

  pub fn extract_abstract_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    let (id, tparams, params, rtp) = self.extract_fn_signature(def_id);
    let class_def = self.get_class_of_method(id);

    let f = self.factory();
    let empty_body = f.NoTree(rtp).into();
    f.FunDef(
      id,
      self.filter_class_tparams(tparams, class_def),
      params,
      rtp,
      empty_body,
      class_def
        .iter()
        .map(|cd| f.IsMethodOf(cd.id).into())
        .chain(iter::once(f.IsAbstract().into()))
        .collect(),
    )
  }

  fn extract_fn_signature(
    &mut self,
    def_id: DefId,
  ) -> (
    &'l SymbolIdentifier<'l>,
    Vec<&'l TypeParameterDef<'l>>,
    Params<'l>,
    st::Type<'l>,
  ) {
    let f = self.factory();

    // Extract the function signature
    let Generics { tparams, txtcx, .. } = self.get_generics(def_id);
    let poly_fn_sig = self.tcx.fn_sig(def_id);
    let fn_sig = self.tcx.liberate_late_bound_regions(def_id, &poly_fn_sig);
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
    let return_tpe = self.extract_ty(fn_sig.output(), &txtcx, DUMMY_SP);

    let fun_id = self.extract_fn_ref(def_id);
    (fun_id, tparams, params, return_tpe)
  }

  /// Extract a local function
  pub(super) fn extract_local_fn(&mut self, fn_item: &FnItem<'l>) -> &'l st::FunDef<'l> {
    let f = self.factory();
    let tcx = self.tcx;

    assert!(fn_item.def_id.is_local());
    let hir_id = tcx.hir().as_local_hir_id(fn_item.def_id.expect_local());
    let class_def = self.get_class_of_method(fn_item.fd_id);

    // Extract flags
    let (carrier_flags, mut flags_by_symbol) = self.extract_flags(hir_id);
    let mut flags = carrier_flags.to_stainless(f);

    // Add flag specifying that this function is a method of its class (if there's a class)
    flags.extend(class_def.into_iter().map(|cd| {
      let flag: st::Flag<'l> = f.IsMethodOf(cd.id).into();
      flag
    }));

    // Extract the function itself
    type Parts<'l> = (Params<'l>, st::Type<'l>, st::Expr<'l>);
    let Generics { tparams, txtcx, .. } = self.get_generics(fn_item.def_id);

    let (params, return_tpe, body_expr): Parts<'l> =
      self.enter_body(hir_id, txtcx.clone(), class_def, |bxtor| {
        // Register parameters and local bindings in the DefContext
        bxtor.populate_def_context(&mut flags_by_symbol);

        // Extract the body
        let body_expr = bxtor.hcx.mirror(&bxtor.body.value);
        let body_expr = bxtor.extract_expr(body_expr);

        (bxtor.body_params(), bxtor.return_tpe(), body_expr)
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

  /// Filter out the tparams of the class and the class itself
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

  /// Extract the definition ID of an ADT.
  ///
  /// If the ADT is already in the mappings, it will reuse the ID, otherwise the
  /// function first parses the ADT, then uses the ID.
  pub(super) fn extract_adt_id(&mut self, def_id: DefId) -> &'l st::SymbolIdentifier<'l> {
    self
      // get known ID
      .get_id_from_def(def_id)
      // otherwise extract ADT, then get the ID
      .unwrap_or_else(|| &self.extract_adt(def_id).id)
  }

  /// Extract an ADT (regardless of whether it is local or external)
  pub(super) fn extract_adt(&mut self, def_id: DefId) -> &'l st::ADTSort<'l> {
    let sort_opt = self.with_extraction(|xt| {
      self
        .get_id_from_def(def_id)
        .and_then(|id| xt.adts.get(id).copied())
    });

    match sort_opt {
      Some(sort) => sort,
      None => {
        let f = self.factory();
        let adt_id = self.get_or_register_def(def_id);
        let adt_def = self.tcx.adt_def(def_id);

        // Extract flags
        let local_def_id_opt = def_id.as_local();

        // Extract flags for local def ids.
        let (flags, mut flags_by_symbol, hir_id_opt) = match local_def_id_opt {
          Some(local_def_id) => {
            let hir_id = self.tcx.hir().as_local_hir_id(local_def_id);
            let (carrier_flags, by_symbol) = self.extract_flags(hir_id);
            (carrier_flags.to_stainless(f), by_symbol, Some(hir_id))
          }
          // TODO: Extract external (non-local) flags. Tracked here:
          //   https://github.com/epfl-lara/rust-stainless/issues/36
          //   Currently, we just return empty flags for non-local ADTs.
          _ => (vec![], HashMap::new(), None),
        };

        // Extract generics
        let Generics { tparams, txtcx, .. } = self.get_generics(def_id);

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

                let flags = flags_by_symbol.remove(&field.ident.name);
                let flags = flags.map(|flags| flags.to_stainless(f)).unwrap_or_default();

                let field = f.Variable(field_id, field_ty, flags);
                &*f.ValDef(field)
              })
              .collect();
            &*f.ADTConstructor(cons_id, adt_id, fields)
          })
          .collect();

        hir_id_opt.map(|hir_id| self.report_unused_flags(hir_id, &flags_by_symbol));
        f.ADTSort(adt_id, tparams, constructors, flags)
      }
    }
  }
}
