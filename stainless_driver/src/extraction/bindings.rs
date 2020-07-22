use super::*;

use rustc_hir::{self as hir, HirId, PatKind};
use rustc_middle::ty;

use stainless_data::ast as st;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  /// Build a DefContext that includes all variable bindings
  pub(super) fn populate_def_context_and_params(&mut self) {
    // Bindings from the body
    BindingsCollector::run(self);

    // Bindings from the params
    for param in self.body.params {
      self.extract_binding(param.pat.hir_id);
    }
  }

  /// Extract a binding based on the binding node's HIR id.
  /// Updates `dcx` if the binding hadn't been extacted before.
  fn extract_binding(&mut self, hir_id: HirId) -> &'l st::Variable<'l> {
    let xtor = &mut self.xtor;
    let tcx = xtor.tcx;
    let dcx = &mut self.dcx;

    match dcx.vars.get(&hir_id) {
      Some(var) => var,
      None => {
        // Extract ident from corresponding HIR node, sanity-check binding mode
        let (id, span) = {
          let node = tcx.hir().find(hir_id).unwrap();
          let ident = if let hir::Node::Binding(pat) = node {
            if let PatKind::Binding(_, _, ident, _) = pat.kind {
              match self
                .tables
                .extract_binding_mode(tcx.sess, pat.hir_id, pat.span)
              {
                Some(ty::BindByValue(hir::Mutability::Not)) => {}
                _ => xtor.unsupported(pat.span, "Only immutable by-value bindings are supported"),
              }
              ident
            } else {
              unreachable!()
            }
          } else {
            let span = node.ident().map(|ident| ident.span).unwrap_or_default();
            xtor.unsupported(
              span,
              "Cannot extract complex pattern in binding (cannot recover from this)",
            );
            unreachable!()
          };
          (
            xtor.register_hir(hir_id, ident.name.to_string()),
            ident.span,
          )
        };

        // Build a Variable node
        // TODO: Extract flags on bindings
        let tpe = xtor.extract_ty(self.tables.node_type(hir_id), &self.dcx, span);
        let var = xtor.factory().Variable(id, tpe, vec![]);
        self.dcx.add_var(hir_id, var);
        var
      }
    }
  }
}

/// DefContext tracks available bindings
#[derive(Debug)]
pub(super) struct DefContext<'l> {
  vars: HashMap<HirId, &'l st::Variable<'l>>,
}

impl<'l> DefContext<'l> {
  pub(super) fn new() -> Self {
    Self {
      vars: HashMap::new(),
    }
  }

  fn add_var(&mut self, hir_id: HirId, var: &'l st::Variable<'l>) -> &mut Self {
    assert!(!self.vars.contains_key(&hir_id));
    self.vars.insert(hir_id, var);
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

impl<'bxtor, 'a, 'l, 'tcx> Visitor<'tcx> for BindingsCollector<'bxtor, 'a, 'l, 'tcx> {
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
        self.bxtor.extract_binding(hir_id);

        // Visit potential sub-patterns
        rustc_ast::walk_list!(self, visit_pat, optional_subpattern);
      }
      _ => intravisit::walk_pat(self, pattern),
    }
  }
}
