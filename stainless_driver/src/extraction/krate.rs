use super::*;

use rustc_hir::def_id::DefId;
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, ItemKind};
use rustc_hir_pretty as pretty;
use rustc_middle::ty::List;
use rustc_span::DUMMY_SP;

use stainless_data::ast as st;

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
          path_str.starts_with("::std::prelude::v") || path_str.starts_with("stainless")
        }
        // TODO: Quick fix to filter our synthetic functions
        // ItemKind::Fn(..) if !item.attrs.is_empty() => true,
        _ => false,
      }
    }

    struct ItemVisitor<'xtor, 'l, 'tcx> {
      xtor: &'xtor mut BaseExtractor<'l, 'tcx>,
      adts: Vec<&'tcx hir::Item<'tcx>>,
      functions: Vec<&'tcx hir::Item<'tcx>>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        match item.kind {
          _ if should_ignore(item) => {}
          ItemKind::Enum(..) | ItemKind::Struct(..) | ItemKind::Fn(..) => {
            let def_id = self.xtor.tcx.hir().local_def_id(item.hir_id).to_def_id();
            let def_path_str = self.xtor.tcx.def_path_str(def_id);
            if let ItemKind::Fn(..) = item.kind {
              eprintln!("  - Fun {}", def_path_str);
              self.functions.push(&item);
            } else {
              eprintln!("  - ADT {}", def_path_str);
              self.adts.push(&item);
            }
          }
          _ => {
            self.xtor.unsupported(item.span, "Other kind of item");
          }
        }
      }

      /// Unsupported

      fn visit_trait_item(&mut self, trait_item: &'tcx hir::TraitItem<'tcx>) {
        self.xtor.unsupported(trait_item.span, "Trait item");
      }

      fn visit_impl_item(&mut self, impl_item: &'tcx hir::ImplItem<'tcx>) {
        self.xtor.unsupported(impl_item.span, "Impl item");
      }
    }

    let krate = self.tcx.hir().krate();

    // Discover items in the local crate
    let mut visitor = ItemVisitor {
      xtor: self,
      adts: vec![],
      functions: vec![],
    };
    eprintln!("[ Discovering local definitions ]");
    krate.visit_all_item_likes(&mut visitor);
    eprintln!("");

    // Extract local items
    let (adts, functions) = (visitor.adts, visitor.functions);
    for item in adts {
      let def_id = self.tcx.hir().local_def_id(item.hir_id).to_def_id();
      let sort = self.extract_adt(def_id);
      self.add_adt(sort.id, sort);
    }
    for item in functions {
      let def_id = self.tcx.hir().local_def_id(item.hir_id).to_def_id();
      let fd = self.extract_local_fn(def_id);
      self.add_function(fd.id, fd);
    }

    // Extract external items as stubs
    let external_functions = self.with_extraction(|xt| {
      // Find all those functions that have been referenced, but not yet extracted
      xt.function_refs
        .iter()
        .filter(|def_id| !def_id.is_local())
        .copied()
        .collect::<Vec<DefId>>()
    });
    for def_id in external_functions {
      let fd = self.extract_extern_fn(def_id);
      self.add_function(fd.id, fd);
    }
  }

  /// Extract a function reference (regardless of whether it is local or external)
  // TODO: Extract flags on functions and parameters
  pub(super) fn extract_fn_ref(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    match self.get_id_from_def(def_id) {
      Some(fun_id) => fun_id,
      None => {
        self.add_function_ref(def_id);
        self.register_def(def_id)
      }
    }
  }

  /// Extract an external function
  pub(super) fn extract_extern_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    let f = self.factory();
    assert!(
      !def_id.is_local(),
      "Expected non-local def id, got: {:?}",
      def_id
    );

    // Extract the function signature
    let (tparams, txtcx) = self.extract_generics(def_id);
    let poly_fn_sig = self.tcx.fn_sig(def_id);
    let fn_sig = self.tcx.liberate_late_bound_regions(def_id, &poly_fn_sig);
    let params: Params<'l> = fn_sig
      .inputs()
      .iter()
      .enumerate()
      .map(|(i, ty)| {
        let id = self.fresh_param_id(i);
        let tpe = self.extract_ty(ty, &txtcx, DUMMY_SP);
        let var = f.Variable(id, tpe, vec![]);
        &*f.ValDef(var)
      })
      .collect();
    let return_tpe = self.extract_ty(fn_sig.output(), &txtcx, DUMMY_SP);

    // Attach an empty body
    let body_expr = f.NoTree(return_tpe).into();

    let flags = vec![f.Extern().into()];
    let fun_id = self.extract_fn_ref(def_id);
    f.FunDef(fun_id, tparams, params, return_tpe, body_expr, flags)
  }

  /// Extract a local function
  pub(super) fn extract_local_fn(&mut self, def_id: DefId) -> &'l st::FunDef<'l> {
    let f = self.factory();
    let tcx = self.tcx;
    assert!(def_id.is_local());

    type Parts<'l> = (Params<'l>, st::Type<'l>, st::Expr<'l>, Vec<st::Flag<'l>>);

    // Extract the function itself
    let (tparams, txtcx) = self.extract_generics(def_id);
    let hir_id = tcx.hir().as_local_hir_id(def_id.expect_local());
    let (params, return_tpe, body_expr, flags): Parts<'l> =
      self.enter_body(hir_id, txtcx.clone(), |bxtor| {
        // Register parameters and local bindings in the DefContext
        bxtor.populate_def_context();

        // Extract the function signature
        let sigs = bxtor.tables.liberated_fn_sigs();
        let sig = sigs.get(hir_id).unwrap();
        let decl = tcx.hir().fn_decl_by_hir_id(hir_id).unwrap();
        let return_tpe = bxtor
          .base
          .extract_ty(sig.output(), &bxtor.txtcx, decl.output.span());

        let params: Params<'l> = bxtor
          .body
          .params
          .iter()
          .map(|param| &*f.ValDef(bxtor.fetch_var(param.pat.hir_id)))
          .collect();

        // Extract the body
        let body_expr = bxtor.hcx.mirror(&bxtor.body.value);
        let body_expr = bxtor.extract_expr(body_expr);
        let flags = vec![];

        (params, return_tpe, body_expr, flags)
      });

    let fun_id = self.extract_fn_ref(def_id);
    f.FunDef(fun_id, tparams, params, return_tpe, body_expr, flags)
  }

  /// Extract an ADT (regardless of whether it is local or external)
  pub(super) fn extract_adt(&mut self, def_id: DefId) -> &'l st::ADTSort<'l> {
    let sort_opt = self.with_extraction(|xt| {
      xt.mapping
        .did_to_stid
        .get(&def_id)
        .and_then(|id| xt.adts.get(id).copied())
    });
    match sort_opt {
      Some(sort) => sort,
      None => {
        let f = self.factory();
        let adt_id = self.register_def(def_id);
        let adt_def = self.tcx.adt_def(def_id);

        let (tparams, txtcx) = self.extract_generics(def_id);

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
                let field_ty = field.ty(self.tcx, List::empty());
                let field_ty = self.extract_ty(field_ty, &txtcx, field.ident.span);
                // TODO: Extract flags on ADT fields
                let field = f.Variable(field_id, field_ty, vec![]);
                &*f.ValDef(field)
              })
              .collect();
            &*f.ADTConstructor(cons_id, adt_id, fields)
          })
          .collect();

        // TODO: Extract flags on ADTs
        let flags = vec![];

        f.ADTSort(adt_id, tparams, constructors, flags)
      }
    }
  }
}
