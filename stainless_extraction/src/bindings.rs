use super::flags::Flags;
use super::*;

use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::{self as hir, HirId, Node, Pat, PatKind};
use rustc_middle::ty;
use rustc_span::symbol::Symbol;

use stainless_data::ast as st;

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
      let (ident, binding_mode, span) = if let Node::Binding(Pat {
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
          span,
        )
      } else {
        xtor.unsupported(
          node.ident().map(|ident| ident.span).unwrap_or_default(),
          "Cannot extract complex pattern in binding (cannot recover from this)",
        );
        unreachable!()
      };

      if let ty::BindByReference(Mutability::Mut) = binding_mode {
        xtor.unsupported(*span, "Only immutable bindings are supported");
      }

      let id = xtor.register_hir(hir_id, ident.name.to_string());
      let mutable = matches!(binding_mode, ty::BindByValue(Mutability::Mut));

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
      _ => self
        .dcx
        .let_var_pairs
        .iter()
        .fold(body_expr, |body, (body_var, &fn_param)| {
          // Mutable ADTs need to be copied freshly to work-around Stainless'
          // anti-aliasing rules. The fresh copy marks them as owned i.e. safe
          // to mutate locally.
          let arg = match fn_param.tpe {
            st::Type::ADTType(_) => f.FreshCopy(fn_param.into()).into(),
            st::Type::TypeParameter(t) if t.is_mutable() => f.FreshCopy(fn_param.into()).into(),
            _ => fn_param.into(),
          };
          f.LetVar(body_var, arg, body).into()
        }),
    }
  }
}

/// DefContext tracks available bindings
#[derive(Clone, Debug, Default)]
pub(super) struct DefContext<'l> {
  vars: HashMap<HirId, &'l st::Variable<'l>>,
  params: Vec<&'l st::ValDef<'l>>,
  let_var_pairs: HashMap<&'l st::ValDef<'l>, &'l st::Variable<'l>>,
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
      let new_param_var = xtor.immutable_var_with_name(vd.v, &format!("var{}", self.params.len()));
      self.params.push(xtor.factory().ValDef(new_param_var));
      self.let_var_pairs.insert(vd, new_param_var);
    } else {
      self.params.push(vd);
    };
    self
  }

  #[inline]
  pub(super) fn get_var(&self, hir_id: HirId) -> Option<&'l st::Variable<'l>> {
    self.vars.get(&hir_id).copied()
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
