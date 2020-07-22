use super::bindings::DefContext;
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
          name == "std" || name == "num_bigint"
        }
        ItemKind::Use(ref path, _) => {
          let path_str = pretty_path(path);
          path_str.starts_with("::std::prelude::v") || path_str.starts_with("num_bigint::")
        }
        // TODO: Quick fix to filter our synthetic functions
        ItemKind::Fn(..) if !item.attrs.is_empty() => true,
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

      // Unsupported
      fn visit_trait_item(&mut self, trait_item: &'tcx hir::TraitItem<'tcx>) {
        self.xtor.unsupported(trait_item.span, "Trait item");
      }

      // Handled above
      fn visit_impl_item(&mut self, _impl_item: &'tcx hir::ImplItem<'tcx>) {
        unreachable!();
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
      self.extract_adt(def_id);
    }
    for item in functions {
      let def_id = self.tcx.hir().local_def_id(item.hir_id).to_def_id();
      self.extract_fn(def_id);
    }

    // Extract external items as stubs
    let external_functions = self.with_extraction(|xt| {
      // Find all those functions that have been referenced, but not yet extracted
      xt.function_refs
        .iter()
        .filter(|def_id| {
          let fun_id = xt.mapping.did_to_stid.get(def_id).unwrap();
          !xt.functions.contains_key(fun_id)
        })
        .copied()
        .collect::<Vec<DefId>>()
    });
    for def_id in external_functions {
      let was_external = self.extract_fn(def_id);
      assert!(was_external);
    }
  }

  /// Extract a function declaration (regardless of whether it is local or external)
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

  /// Extract a function
  fn extract_fn(&mut self, def_id: DefId) -> bool {
    let f = self.factory();

    let is_external = !def_id.is_local();
    let span = self.tcx.span_of_impl(def_id).unwrap_or(DUMMY_SP);

    let generics = self.tcx.generics_of(def_id);
    if generics.count() > 0 {
      self.unsupported(
        span,
        format!(
          "Type parameters on functions are unsupported: {:#?}",
          generics
        ),
      );
      return is_external;
    }

    // Extract the function signature
    type Parts<'l> = (Params<'l>, st::Type<'l>, st::Expr<'l>, Vec<st::Flag<'l>>);

    let (params, return_tpe, body_expr, flags): Parts<'l> = if is_external {
      let poly_fn_sig = self.tcx.fn_sig(def_id);
      let fn_sig = self.tcx.liberate_late_bound_regions(def_id, &poly_fn_sig);
      let dcx = DefContext::new();
      let params: Params<'l> = fn_sig
        .inputs()
        .iter()
        .enumerate()
        .map(|(i, ty)| {
          let id = self.fresh_param_id(i);
          let tpe = self.extract_ty(ty, &dcx, DUMMY_SP);
          let var = f.Variable(id, tpe, vec![]);
          &*f.ValDef(var)
        })
        .collect();
      let return_tpe = self.extract_ty(fn_sig.output(), &dcx, DUMMY_SP);

      // Attach an empty body
      let body_expr = f.NoTree(return_tpe).into();
      let flags = vec![f.Extern().into()];

      (params, return_tpe, body_expr, flags)
    } else {
      let tcx = self.tcx;
      let hir_id = tcx.hir().as_local_hir_id(def_id.expect_local());
      self.enter_body(hir_id, |bxtor| {
        // Extract the function signature and extract the signature
        let sigs = bxtor.tables.liberated_fn_sigs();
        let sig = sigs.get(hir_id).unwrap();
        let decl = tcx.hir().fn_decl_by_hir_id(hir_id).unwrap();
        let return_tpe = bxtor.extract_ty(sig.output(), decl.output.span());

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
      })
    };

    let fun_id = self.extract_fn_ref(def_id);
    let fd = f.FunDef(fun_id, vec![], params, return_tpe, body_expr, flags);
    self.add_function(fun_id, fd);
    is_external
  }

  /// Extract an ADT (regardless of whether it is local or external)
  pub(super) fn extract_adt(&mut self, def_id: DefId) -> StainlessSymId<'l> {
    match self.get_id_from_def(def_id) {
      Some(adt_id) => adt_id,
      None => {
        let f = self.factory();
        let adt_id = self.register_def(def_id);
        let adt_def = self.tcx.adt_def(def_id);
        let span = self.tcx.span_of_impl(def_id).unwrap_or(DUMMY_SP);

        // TODO: Support type parameters on ADTs
        // TODO: Extract flags on ADTs
        let generics = self.tcx.generics_of(def_id);
        if generics.count() > 0 {
          self.unsupported(span, "Type parameters on ADT are unsupported");
          unreachable!()
        }

        let constructors = adt_def
          .variants
          .iter()
          .map(|variant| {
            let cons_id = self.register_def(variant.def_id);
            let fields = variant
              .fields
              .iter()
              .map(|field| {
                let field_id = self.register_def(field.did);
                let field_ty = field.ty(self.tcx, List::empty());
                let field_ty = self.extract_ty(field_ty, &DefContext::new(), field.ident.span);
                // TODO: Extract flags on ADT fields
                let field = f.Variable(field_id, field_ty, vec![]);
                &*f.ValDef(field)
              })
              .collect();
            &*f.ADTConstructor(cons_id, adt_id, fields)
          })
          .collect();

        let adt = f.ADTSort(adt_id, vec![], constructors, vec![]);
        self.add_adt(adt_id, adt);
        adt_id
      }
    }
  }
}
