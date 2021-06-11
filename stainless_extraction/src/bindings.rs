use super::flags::Flags;
use super::*;

use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::{self as hir, def, BorrowKind, HirId, ImplicitSelfKind, Node, Pat, PatKind};
use rustc_middle::ty;
use rustc_span::symbol::Symbol;

use stainless_data::ast as st;
use std::iter;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  /// Build a DefContext that includes all variable bindings
  pub(super) fn populate_def_context(
    &mut self,
    flags_by_symbol: &mut HashMap<Symbol, Flags>,
    add_params: &[&'l st::ValDef<'l>],
  ) {
    // Bindings from the params
    for param in self.body.params {
      let flags = param
        .pat
        .simple_ident()
        .and_then(|ident| flags_by_symbol.remove(&ident.name));
      let var = self.get_or_extract_binding(param.pat.hir_id, flags);
      self
        .dcx
        .add_param(self.factory().ValDef(var), &mut self.base);
    }

    // Additional parameters (usually evidence parameters)
    for v in add_params {
      self.dcx.add_param(v, &mut self.base);
    }

    // Bindings from the body
    BindingsCollector::run(self);
  }

  /// Extract a binding based on the binding node's HIR id.
  /// Updates `dcx` if the binding hadn't been extracted before.
  fn get_or_extract_binding(
    &mut self,
    hir_id: HirId,
    flags_opt: Option<Flags>,
  ) -> &'l st::Variable<'l> {
    self.dcx.get_var(hir_id).unwrap_or_else(|| {
      let xtor = &mut self.base;

      // Extract ident from corresponding HIR node, sanity-check binding mode

      let node = xtor.tcx.hir().find(hir_id).unwrap();
      let (ident, binding_mode) = if let Node::Binding(Pat {
        kind: PatKind::Binding(_, _, ident, _),
        hir_id,
        span,
        ..
      }) = node
      {
        (
          ident,
          self
            .tables
            .extract_binding_mode(xtor.tcx.sess, *hir_id, *span)
            .expect("Cannot extract binding without binding mode."),
        )
      } else {
        xtor.unsupported(
          node.ident().map(|ident| ident.span).unwrap_or_default(),
          "Cannot extract complex pattern in binding (cannot recover from this)",
        );
        unreachable!()
      };

      let id = xtor.register_hir(hir_id, ident.name.to_string());
      let mutable = matches!(
        binding_mode,
        ty::BindByValue(hir::Mutability::Mut) | ty::BindByReference(hir::Mutability::Mut)
      );

      // Build a Variable node
      let f = xtor.factory();
      let tpe = xtor.extract_ty(self.tables.node_type(hir_id), &self.txtcx, ident.span);
      let flags = flags_opt
        .map(|flags| flags.to_stainless(f))
        .into_iter()
        .flatten()
        // Add @var flag if the param is mutable
        .chain(mutable.then(|| f.IsVar().into()))
        .collect();
      let var = f.Variable(id, tpe, flags);
      self.dcx.add_var(hir_id, var);
      var
    })
  }

  /// Wraps the body expression into a LetVar for each mutable parameter of the
  /// function recorded in the DefContext. Because, specs (Ensuring, Require,
  /// Decreases) trees have to be the outermost trees, this also unpacks the
  /// specs and repacks them around the body wrapped in LetVars.
  pub fn wrap_body_let_vars(&self, body_expr: st::Expr<'l>) -> st::Expr<'l> {
    let f = self.factory();
    match body_expr {
      st::Expr::Ensuring(st::Ensuring { body, pred }) => {
        let wrapped_body = self.wrap_body_let_vars(*body);
        f.Ensuring(wrapped_body, pred).into()
      }
      st::Expr::Require(st::Require { body, pred }) => {
        let wrapped_body = self.wrap_body_let_vars(*body);
        f.Require(*pred, wrapped_body).into()
      }
      st::Expr::Decreases(st::Decreases { measure, body }) => {
        let wrapped_body = self.wrap_body_let_vars(*body);
        f.Decreases(*measure, wrapped_body).into()
      }
      _ => self.dcx.let_var_pairs.iter().fold(
        body_expr,
        |body, (body_var, &(fn_param, opt_hir_id))| {
          // Mutable ADTs need to be copied freshly to work-around Stainless'
          // anti-aliasing rules. The fresh copy marks them as owned i.e. safe
          // to mutate locally. The exception is for types that contain mutable
          // references.
          let is_mutable = opt_hir_id.map_or(false, |id| is_mutable(self.tables.node_type(id)));

          f.LetVar(
            body_var,
            if is_mutable {
              fn_param.into()
            } else {
              self
                .base
                .fresh_copy_if_needed(fn_param.into(), fn_param.tpe)
            },
            body,
          )
          .into()
        },
      ),
    }
  }
}

/// DefContext tracks available bindings
#[derive(Clone, Debug, Default)]
pub(super) struct DefContext<'l> {
  vars: HashMap<HirId, &'l st::Variable<'l>>,
  params: Vec<&'l st::ValDef<'l>>,
  let_var_pairs: HashMap<&'l st::ValDef<'l>, (&'l st::Variable<'l>, Option<HirId>)>,
}

impl<'l> DefContext<'l> {
  pub(super) fn params(&self) -> &[&'l st::ValDef<'l>] {
    &self.params[..]
  }

  pub(super) fn add_var(&mut self, hir_id: HirId, var: &'l st::Variable<'l>) -> &mut Self {
    assert!(!self.vars.contains_key(&hir_id));
    self.vars.insert(hir_id, var);
    self
  }

  /// Adds a parameter to the available bindings.
  ///
  /// If the parameter is mutable, a new immutable parameter is inserted in the
  /// parameter list. The binding with a LetVar from the new param to the
  /// variable in the function's body needs to be created later with
  /// [BodyExtractor::wrap_body_let_vars].
  pub(super) fn add_param(
    &mut self,
    vd: &'l st::ValDef<'l>,
    xtor: &mut BaseExtractor<'l, '_>,
  ) -> &mut Self {
    assert!(!self.params.contains(&vd));
    if vd.is_mutable() {
      let original_id = self
        .vars
        .iter()
        .find(|(_, &v)| v == vd.v)
        .map(|(id, _)| *id);

      let new_param_var = xtor.immutable_var_with_name(vd.v, &format!("var{}", self.params.len()));
      self.params.push(xtor.factory().ValDef(new_param_var));
      self.let_var_pairs.insert(vd, (new_param_var, original_id));
    } else {
      self.params.push(vd);
    };
    self
  }

  #[inline]
  pub(super) fn get_var(&self, hir_id: HirId) -> Option<&'l st::Variable<'l>> {
    self.vars.get(&hir_id).copied()
  }

  pub(super) fn make_mut_cell(&mut self, hir_id: HirId, xtor: &mut BaseExtractor<'l, '_>) {
    self.vars.entry(hir_id).and_modify(|v| {
      if !xtor.is_mut_cell(v.tpe) {
        let f = xtor.factory();
        let tpe = xtor.synth().mut_cell_type(v.tpe);
        *v = f.Variable(
          v.id,
          tpe,
          v.flags
            .iter()
            .copied()
            .chain(iter::once(f.wrapped_flag()))
            .collect(),
        )
      }
    });
  }
}

/// BindingsCollector populates a DefContext
struct BindingsCollector<'bxtor, 'a, 'l, 'tcx> {
  bxtor: &'bxtor mut BodyExtractor<'a, 'l, 'tcx>,
}

impl<'bxtor, 'a, 'l, 'tcx> BindingsCollector<'bxtor, 'a, 'l, 'tcx> {
  fn run(bxtor: &'bxtor mut BodyExtractor<'a, 'l, 'tcx>) -> Self {
    let body = bxtor.body;
    assert!(body.generator_kind.is_none());

    let mut this = Self { bxtor };
    for param in body.params {
      this.visit_pat(&param.pat);
    }
    this.visit_expr(&body.value);
    this
  }
}

impl<'tcx> Visitor<'tcx> for BindingsCollector<'_, '_, '_, 'tcx> {
  type Map = rustc_middle::hir::map::Map<'tcx>;

  fn nested_visit_map(&mut self) -> NestedVisitorMap<Self::Map> {
    NestedVisitorMap::None
  }

  fn visit_body(&mut self, _b: &'tcx hir::Body<'tcx>) {
    unreachable!();
  }

  fn visit_expr(&mut self, expr: &'tcx hir::Expr<'tcx>) {
    // We promote a local variable to a MutCell
    match expr.kind {
      // if it is later mutably borrowed
      hir::ExprKind::AddrOf(BorrowKind::Ref, Mutability::Mut, expr) => {
        if let Some(id) = id_from_path_expr(expr) {
          self.bxtor.dcx.make_mut_cell(id, &mut self.bxtor.base)
        }
      }

      // if it's the receiver of a method with implicit `&mut self`
      hir::ExprKind::MethodCall(_, _, args, ..) => {
        if let Some(ImplicitSelfKind::MutRef) = self.implicit_self_of_call(expr.hir_id) {
          if let Some(id) = id_from_path_expr(&args[0]) {
            self.bxtor.dcx.make_mut_cell(id, &mut self.bxtor.base)
          }
        }
      }
      _ => {}
    };
    // Visit subexpressions
    intravisit::walk_expr(self, expr)
  }

  fn visit_pat(&mut self, pattern: &'tcx hir::Pat<'tcx>) {
    match pattern.kind {
      hir::PatKind::Binding(_, hir_id, ref _ident, ref optional_subpattern) => {
        // Extend DefContext with a new variable
        self.bxtor.get_or_extract_binding(hir_id, None);

        // Visit potential sub-patterns
        rustc_ast::walk_list!(self, visit_pat, optional_subpattern);
      }
      _ => intravisit::walk_pat(self, pattern),
    }
  }
}

impl BindingsCollector<'_, '_, '_, '_> {
  fn implicit_self_of_call(&self, hir_id: HirId) -> Option<ImplicitSelfKind> {
    let tcx = self.bxtor.tcx();
    let parent_did = tcx.hir().get_parent_did(hir_id);
    let called_fn_did = tcx.typeck(parent_did).type_dependent_def_id(hir_id)?;
    let called_fn_hid = tcx.hir().local_def_id_to_hir_id(called_fn_did.as_local()?);
    let decl = tcx.hir().fn_decl_by_hir_id(called_fn_hid)?;
    Some(decl.implicit_self)
  }
}

fn id_from_path_expr(expr: &hir::Expr) -> Option<HirId> {
  match expr {
    hir::Expr {
      kind:
        hir::ExprKind::Path(hir::QPath::Resolved(
          _,
          hir::Path {
            res: def::Res::Local(id),
            ..
          },
        )),
      ..
    } => Some(*id),
    _ => None,
  }
}
