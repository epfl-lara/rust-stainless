use super::flags::Flags;
use super::*;

use rustc_hir::{self as hir, HirId, Node, Pat, PatKind};
use rustc_middle::ty;
use rustc_span::symbol::Symbol;

use stainless_data::ast as st;

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  /// Build a DefContext that includes all variable bindings
  pub(super) fn populate_def_context(
    &mut self,
    flags_by_symbol: &mut HashMap<Symbol, Flags>,
    add_params: &Vec<&'l st::ValDef<'l>>,
  ) {
    // Bindings from the params
    for param in self.body.params {
      let flags = param
        .pat
        .simple_ident()
        .and_then(|ident| flags_by_symbol.remove(&ident.name));
      let (var, _) = self.extract_binding(param.pat.hir_id, flags);
      self.dcx.add_param(self.factory().ValDef(var));
    }

    // Additional parameters (usually evidence parameters)
    for v in add_params {
      self.dcx.add_param(v);
    }

    // Bindings from the body
    BindingsCollector::run(self);
  }

  /// Extract a binding based on the binding node's HIR id.
  /// Updates `dcx` if the binding hadn't been extracted before.
  fn extract_binding(
    &mut self,
    hir_id: HirId,
    flags: Option<Flags>,
  ) -> (&'l st::Variable<'l>, bool) {
    self.dcx.get_var(hir_id).unwrap_or_else(|| {
      let xtor = &mut self.base;

      // Extract ident from corresponding HIR node, sanity-check binding mode
      let (id, span, mutable) = {
        let node = xtor.tcx.hir().find(hir_id).unwrap();

        let (ident, mutable) = if let Node::Binding(Pat {
          kind: PatKind::Binding(_, _, ident, _),
          hir_id,
          span,
        }) = node
        {
          match self
            .tables
            .extract_binding_mode(xtor.tcx.sess, *hir_id, *span)
          {
            // allowed binding modes
            Some(ty::BindByValue(hir::Mutability::Not))
            | Some(ty::BindByReference(hir::Mutability::Not)) => (ident, false),
            Some(ty::BindByValue(hir::Mutability::Mut)) => (ident, true),

            // For the forbidden binding modes, return the identifier anyway
            // because failure will occur later.
            _ => {
              xtor.unsupported(*span, "Only immutable bindings are supported");
              (ident, false)
            }
          }
        } else {
          xtor.unsupported(
            node.ident().map(|ident| ident.span).unwrap_or_default(),
            "Cannot extract complex pattern in binding (cannot recover from this)",
          );
          unreachable!()
        };

        (
          xtor.register_hir(hir_id, ident.name.to_string()),
          ident.span,
          mutable,
        )
      };

      // Build a Variable node
      let tpe = xtor.extract_ty(self.tables.node_type(hir_id), &self.txtcx, span);
      let flags = flags
        .map(|flags| flags.to_stainless(xtor.factory()))
        .unwrap_or_default();
      let var = xtor.factory().Variable(id, tpe, flags);
      self.dcx.add_var(hir_id, var, mutable);
      (var, mutable)
    })
  }
}

/// DefContext tracks available bindings
#[derive(Clone, Debug)]
pub(super) struct DefContext<'l> {
  vars: HashMap<HirId, (&'l st::Variable<'l>, bool)>,
  params: Vec<&'l st::ValDef<'l>>,
}

impl<'l> DefContext<'l> {
  pub(super) fn new() -> Self {
    Self {
      vars: HashMap::new(),
      params: vec![],
    }
  }

  pub(super) fn params(&self) -> &[&'l st::ValDef<'l>] {
    &self.params[..]
  }

  pub(super) fn add_var(
    &mut self,
    hir_id: HirId,
    var: &'l st::Variable<'l>,
    mutable: bool,
  ) -> &mut Self {
    assert!(!self.vars.contains_key(&hir_id));
    self.vars.insert(hir_id, (var, mutable));
    self
  }

  pub(super) fn add_param(&mut self, val: &'l st::ValDef<'l>) -> &mut Self {
    assert!(!self.params.contains(&val));
    self.params.push(val);
    self
  }

  #[inline]
  pub(super) fn get_var(&self, hir_id: HirId) -> Option<(&'l st::Variable<'l>, bool)> {
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
        self.bxtor.extract_binding(hir_id, None);

        // Visit potential sub-patterns
        rustc_ast::walk_list!(self, visit_pat, optional_subpattern);
      }
      _ => intravisit::walk_pat(self, pattern),
    }
  }
}
