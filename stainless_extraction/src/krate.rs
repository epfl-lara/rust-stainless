use super::*;

use rustc_hir::def_id::DefId;
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, ItemKind};
use rustc_hir_pretty as pretty;
use rustc_middle::ty::{DefIdTree, List};
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

    let (adts, mut functions) = (visitor.adts, visitor.functions);

    // Correlate spec functions with the function they specify
    let mut pre_spec_functions: HashMap<DefId, Vec<DefId>> = HashMap::new();
    let mut post_spec_functions: HashMap<DefId, Vec<DefId>> = HashMap::new();
    functions.retain(|item| {
      let def_id = self.tcx.hir().local_def_id(item.hir_id).to_def_id();
      if item.span.from_expansion() {
        let ident_str = item.ident.as_str();
        if let Some(parent_def_id) = self.tcx.parent(def_id) {
          if ident_str.starts_with("__pre") {
            pre_spec_functions
              .entry(parent_def_id)
              .or_insert_with(Vec::new)
              .push(def_id);
            return false;
          } else if ident_str.starts_with("__post") {
            post_spec_functions
              .entry(parent_def_id)
              .or_insert_with(Vec::new)
              .push(def_id);
            return false;
          }
        }
      }
      true
    });

    // Extract local items
    for item in adts {
      let def_id = self.tcx.hir().local_def_id(item.hir_id).to_def_id();
      let sort = self.extract_adt(def_id);
      self.add_adt(sort.id, sort);
    }
    for item in functions {
      let def_id = self.tcx.hir().local_def_id(item.hir_id).to_def_id();
      let pre_spec_functions = pre_spec_functions.remove(&def_id);
      let post_spec_functions = post_spec_functions.remove(&def_id);
      let fd = self.extract_local_fn(def_id, pre_spec_functions, post_spec_functions);
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
        let id = self.fresh_id(format!("param{}", i));
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
  pub(super) fn extract_local_fn(
    &mut self,
    def_id: DefId,
    pre_spec_functions: Option<Vec<DefId>>,
    post_spec_functions: Option<Vec<DefId>>,
  ) -> &'l st::FunDef<'l> {
    let f = self.factory();
    let tcx = self.tcx;
    assert!(def_id.is_local());
    let hir_id = tcx.hir().as_local_hir_id(def_id.expect_local());

    // Extract flags
    let (carrier_flags, mut flags_by_symbol) = self.extract_flags(hir_id);
    let flags = carrier_flags.to_stainless(f);

    // Extract the function itself
    type Parts<'l> = (Params<'l>, st::Type<'l>, st::Expr<'l>);
    let (tparams, txtcx) = self.extract_generics(def_id);
    let (params, return_tpe, mut body_expr): Parts<'l> =
      self.enter_body(hir_id, txtcx.clone(), |bxtor| {
        // Register parameters and local bindings in the DefContext
        bxtor.populate_def_context(&mut flags_by_symbol);

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

        (params, return_tpe, body_expr)
      });

    self.report_unused_flags(hir_id, &flags_by_symbol);

    // Extract specs, if any, and wrap them around the body
    let make_and = |mut exprs: Vec<st::Expr<'l>>| {
      if exprs.len() > 1 {
        f.And(exprs).into()
      } else {
        exprs.pop().unwrap()
      }
    };

    if let Some(pre_spec_functions) = pre_spec_functions {
      let spec_exprs = pre_spec_functions
        .into_iter()
        .map(|spec_def_id| self.extract_spec_fn(spec_def_id, &txtcx, &params, None))
        .collect();
      body_expr = f.Require(make_and(spec_exprs), body_expr).into();
    }

    if let Some(post_spec_functions) = post_spec_functions {
      let return_var = &*f.Variable(self.fresh_id("ret".into()), return_tpe, vec![]);
      let return_vd = f.ValDef(return_var);
      let spec_exprs = post_spec_functions
        .into_iter()
        .map(|spec_def_id| self.extract_spec_fn(spec_def_id, &txtcx, &params, Some(return_var)))
        .collect();
      body_expr = f
        .Ensuring(body_expr, f.Lambda(vec![return_vd], make_and(spec_exprs)))
        .into();
    }

    // Wrap it all up in a Stainless function
    let fun_id = self.extract_fn_ref(def_id);
    f.FunDef(fun_id, tparams, params, return_tpe, body_expr, flags)
  }

  /// Extract a specification function and return its body.
  fn extract_spec_fn(
    &mut self,
    def_id: DefId,
    original_txtcx: &TyExtractionCtxt<'l>,
    original_params: &[&'l st::ValDef<'l>],
    return_var: Option<&'l st::Variable<'l>>,
  ) -> st::Expr<'l> {
    // Spec functions are inner functions within the actual function being specified.
    // They take their own parameters, though those parameters are in a one-to-one correspondence
    // to the surrounding function's parameters. The analogous thing applies to type parameters.
    // Here we try to match all of them up and coerce the extraction to directly translate them to
    // the surrounding function's variables, instead of extracting new, unrelated identifiers.
    let tcx = self.tcx;
    let hir_id = tcx.hir().as_local_hir_id(def_id.expect_local());

    // Correlate type parameters
    let generics = tcx.generics_of(def_id);
    assert_eq!(
      generics.parent,
      tcx.generics_of(original_txtcx.def_id).parent
    );
    let original_index_to_tparam = &original_txtcx.index_to_tparam;
    assert_eq!(generics.params.len(), original_index_to_tparam.len());
    let index_to_tparam = generics
      .params
      .iter()
      .map(|param| (param.index, original_index_to_tparam[&param.index].clone()))
      .collect();
    let txtcx = TyExtractionCtxt {
      def_id,
      index_to_tparam,
    };

    self.enter_body(hir_id, txtcx, |bxtor| {
      // Correlate term parameters
      let mut param_hir_ids: Vec<HirId> = bxtor
        .body
        .params
        .iter()
        .map(|param| param.pat.hir_id)
        .collect();
      let return_hir_id = if return_var.is_some() {
        Some(
          param_hir_ids
            .pop()
            .expect("No return parameter on post spec function"),
        )
      } else {
        None
      };

      assert_eq!(original_params.len(), param_hir_ids.len());
      for (vd, hir_id) in original_params.iter().zip(param_hir_ids) {
        bxtor.dcx.add_var(hir_id, vd.v);
      }

      // Preregister `ret` binding with return_id, if any
      if let Some(return_var) = return_var {
        bxtor.dcx.add_var(return_hir_id.unwrap(), return_var);
      }

      // Pick up any additional local bindings
      bxtor.populate_def_context(&mut HashMap::new());

      // Extract the spec function's body
      let body_expr = bxtor.hcx.mirror(&bxtor.body.value);
      bxtor.extract_expr(body_expr)
    })
  }

  /// Extract the definition ID of an ADT.
  ///
  /// If the ADT is already in the mappings, it will reuse the ID, otherwise the
  /// function first parses the ADT, then uses the ID.
  pub(super) fn extract_adt_id(&mut self, def_id: DefId) -> &'l st::SymbolIdentifier<'l> {
    self
      // get known ID
      .with_extraction(|xt| xt.mapping.did_to_stid.get(&def_id).copied())
      // otherwise extract ADT, then get the ID
      .unwrap_or_else(|| &self.extract_adt(def_id).id)
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

        // Extract flags
        let hir_id = self.tcx.hir().as_local_hir_id(def_id.expect_local());
        let (carrier_flags, mut flags_by_symbol) = self.extract_flags(hir_id);
        let flags = carrier_flags.to_stainless(f);

        // Extract generics
        let (tparams, txtcx) = self.extract_generics(def_id);

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

        self.report_unused_flags(hir_id, &flags_by_symbol);

        f.ADTSort(adt_id, tparams, constructors, flags)
      }
    }
  }
}
