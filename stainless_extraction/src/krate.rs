use super::*;

use rustc_hir::def_id::DefId;
use rustc_hir::itemlikevisit::ItemLikeVisitor;
use rustc_hir::{self as hir, AssocItemKind, ImplItemKind, ItemKind};
use rustc_hir_pretty as pretty;
use rustc_middle::ty::{DefIdTree, List};
use rustc_span::symbol::Ident;
use rustc_span::DUMMY_SP;

use crate::spec::SpecType;
use crate::ty::all_generic_params_of;
use stainless_data::ast as st;

/// Internal data type representing functions in either impl blocks, traits or
/// top-level scope. It can be seen as the subset of the common fields of
/// `rustc_hir::ItemKind::Fn` and `rustc_hir::ImplItemKind::Fn`/
/// `rustc_hir::TraitItemKind::Fn` that we make use of.
///
#[derive(Copy, Clone)]
struct FnItem {
  def_id: DefId,
  span: Span,
  /// The type of the spec, if this is a spec function.
  spec_type: Option<SpecType>,

  /// The name of the corresponding function that is spec'd by this spec
  /// function, if this is a spec function.
  spec_fn_name: Option<Ident>,
}

impl FnItem {
  /// Create a new FnItem by parsing its identifier and setting its spec
  /// function properties; if it's a spec function.
  ///
  /// Note that we currently don't store the identifier once it's parsed. This
  /// can be changed with a one-liner in the struct definition though.
  fn new(def_id: DefId, ident: Ident, span: Span) -> Self {
    match SpecType::parse_spec_type_fn_name(&ident.as_str()) {
      Some((spec_type, spec_fn_name)) => FnItem {
        def_id,
        span,
        spec_type: Some(spec_type),
        spec_fn_name: Some(Ident::from_str(&spec_fn_name)),
      },
      None => FnItem {
        def_id,
        span,
        spec_type: None,
        spec_fn_name: None,
      },
    }
  }

  fn is_spec_fn(&self) -> bool {
    self.span.from_expansion() && self.spec_type.is_some()
  }
}

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
      functions: Vec<FnItem>,
      // Maps the user-function to its spec functions
      specs: HashMap<DefId, Vec<FnItem>>,
    }

    impl<'xtor, 'l, 'tcx> ItemLikeVisitor<'tcx> for ItemVisitor<'xtor, 'l, 'tcx> {
      fn visit_item(&mut self, item: &'tcx hir::Item<'tcx>) {
        match item.kind {
          // Ignore use and external crates, see #should_ignore.
          _ if should_ignore(item) => {}

          // Store enums, structs and top-level functions into adts and functions
          ItemKind::Enum(..) | ItemKind::Struct(..) | ItemKind::Fn(..) => {
            let def_id = self.xtor.tcx.hir().local_def_id(item.hir_id).to_def_id();
            let def_path_str = self.xtor.tcx.def_path_str(def_id);

            match item {
              // Fn case
              hir::Item {
                ident,
                kind: ItemKind::Fn(..),
                span,
                ..
              } => {
                let fn_item = FnItem::new(def_id, *ident, *span);

                // then check the parent and
                match self.xtor.tcx.parent(fn_item.def_id) {
                  // extract a spec function, if there is a parent
                  Some(parent_def_id) if fn_item.is_spec_fn() => self
                    .specs
                    .entry(parent_def_id)
                    .or_insert_with(Vec::new)
                    .push(fn_item),

                  // otherwise, extract a normal function.
                  _ => {
                    eprintln!("  - Fun {}", def_path_str);
                    self.functions.push(fn_item);
                  }
                }
              }

              // ADT case
              _ => {
                eprintln!("  - ADT {}", def_path_str);
                let sort = self.xtor.extract_adt(def_id);
                self.xtor.add_adt(sort.id, sort);
              }
            }
          }

          // Store functions of impl blocks and their specs
          ItemKind::Impl { items, .. } => {
            // Get all functions in the impl by their identifier
            let fns_by_identifier: HashMap<Ident, FnItem> = items
              .into_iter()
              .filter_map(|item| match &item.kind {
                AssocItemKind::Fn { .. } => Some((
                  item.ident,
                  FnItem::new(
                    self.xtor.tcx.hir().local_def_id(item.id.hir_id).to_def_id(),
                    item.ident,
                    item.span,
                  ),
                )),
                // ignore consts and type aliases in impl blocks
                _ => None,
              })
              .collect();

            let (specs, fns): (Vec<&FnItem>, Vec<&FnItem>) = fns_by_identifier
              .values()
              .partition(|&fn_item| fn_item.is_spec_fn());
            self.functions.extend(fns);

            specs.iter().for_each(|&&spec_item| {
              if let Some(fn_item) = spec_item
                .spec_fn_name
                // retrieve corresponding fn_item from the map
                .and_then(|fn_ident| fns_by_identifier.get(&fn_ident))
              {
                self
                  .specs
                  .entry(fn_item.def_id)
                  .or_insert_with(Vec::new)
                  .push(spec_item)
              }
            });
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

      /// Ignore fn items in because they are already treated when the entire
      /// impl/trait block is extracted.
      fn visit_impl_item(&mut self, impl_item: &'tcx hir::ImplItem<'tcx>) {
        match impl_item {
          hir::ImplItem {
            kind: ImplItemKind::Fn(..),
            ..
          } => {}
          _ => self
            .xtor
            .unsupported(impl_item.span, "Impl item other than function"),
        }
      }
    }

    let krate = self.tcx.hir().krate();

    // Discover items in the local crate
    let mut visitor = ItemVisitor {
      xtor: self,
      functions: vec![],
      specs: HashMap::new(),
    };
    eprintln!("[ Discovering local definitions ]");
    krate.visit_all_item_likes(&mut visitor);
    eprintln!();

    let ItemVisitor {
      functions, specs, ..
    } = visitor;

    for fn_item in functions {
      let fn_specs = specs
        .get(&fn_item.def_id)
        .into_iter()
        .flatten()
        .filter_map(|spec_item| spec_item.spec_type.map(|s| (s, spec_item.def_id)));

      let (measure_fns, other_spec_fns): (Vec<(SpecType, DefId)>, Vec<(SpecType, DefId)>) =
        fn_specs.partition(|(spec_type, _)| spec_type == &SpecType::Measure);

      if measure_fns.len() > 1 {
        self.tcx.sess.span_err(fn_item.span, "Multiple measures.");
      }

      let (pre_fns, post_fns): (Vec<(SpecType, DefId)>, Vec<(SpecType, DefId)>) = other_spec_fns
        .into_iter()
        .partition(|(spec_type, _)| spec_type == &SpecType::Pre);

      let fd = self.extract_local_fn(
        fn_item.def_id,
        pre_fns.iter().map(|(_, def_id)| *def_id).collect(),
        post_fns.iter().map(|(_, def_id)| *def_id).collect(),
        measure_fns.first().map(|(_, def_id)| *def_id),
      );
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
    pre_spec_functions: Vec<DefId>,
    post_spec_functions: Vec<DefId>,
    measure_spec_function: Option<DefId>,
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

    if let Some(measure_spec_def_id) = measure_spec_function {
      // The measure function generated by the macro has a Unit return type to
      // deal with the unknown type of the measure. The expression has a
      // trailing ';' (UnitLiteral) to implement this. Here, we need to remove
      // the UnitLiteral and actually return the measure in the block
      // expression.
      if let st::Expr::Block(st::Block {
        exprs,
        last: st::Expr::UnitLiteral(st::UnitLiteral {}),
      }) = self.extract_spec_fn(measure_spec_def_id, &txtcx, &params, None)
      {
        // Create a block that returns its last expression
        let (last, other_exprs) = exprs.split_last().expect("No measure provided.");

        body_expr = f
          .Decreases(
            st::Expr::Block(f.Block(other_exprs.to_vec(), *last)),
            body_expr,
          )
          .into();
      }
    }

    let spec_exprs = pre_spec_functions
      .into_iter()
      .map(|spec_def_id| self.extract_spec_fn(spec_def_id, &txtcx, &params, None))
      .collect::<Vec<_>>();
    if !spec_exprs.is_empty() {
      body_expr = f.Require(f.make_and(spec_exprs), body_expr).into();
    }

    let return_var = &*f.Variable(self.fresh_id("ret".into()), return_tpe, vec![]);
    let return_vd = f.ValDef(return_var);
    let spec_exprs = post_spec_functions
      .into_iter()
      .map(|spec_def_id| self.extract_spec_fn(spec_def_id, &txtcx, &params, Some(return_var)))
      .collect::<Vec<_>>();
    if !spec_exprs.is_empty() {
      body_expr = f
        .Ensuring(body_expr, f.Lambda(vec![return_vd], f.make_and(spec_exprs)))
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

    // Correlate type parameters
    let generics = self.tcx.generics_of(def_id);
    let generic_params = all_generic_params_of(self.tcx, def_id);
    assert_eq!(
      generics.parent,
      self.tcx.generics_of(original_txtcx.def_id).parent
    );

    let original_index_to_tparam = &original_txtcx.index_to_tparam;
    assert_eq!(generic_params.len(), original_index_to_tparam.len());
    let index_to_tparam = generic_params
      .iter()
      .map(|param| (param.index, original_index_to_tparam[&param.index].clone()))
      .collect();

    let txtcx = TyExtractionCtxt {
      def_id,
      index_to_tparam,
    };

    let hir_id = self.tcx.hir().as_local_hir_id(def_id.expect_local());
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
          // Currently, we just return empty flags for non-local ADTs.
          _ => (vec![], HashMap::new(), None),
        };

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

        hir_id_opt.map(|hir_id| self.report_unused_flags(hir_id, &flags_by_symbol));
        f.ADTSort(adt_id, tparams, constructors, flags)
      }
    }
  }
}
