use super::flags::Flags;
use super::*;

use rustc_hir::intravisit::{self, NestedVisitorMap, Visitor};
use rustc_hir::{self as hir, HirId, Node, Pat, PatKind};
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
      self.dcx.add_param(self.factory().ValDef(var));
    }

    // Additional parameters (usually evidence parameters)
    for vd in add_params {
      self.dcx.add_param(vd);
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
      let f = xtor.factory();

      // Extract identifier and mutability of the binding from corresponding HIR node
      let (ident, is_mutable) = match xtor.tcx.hir().find(hir_id).unwrap() {
        Node::Binding(Pat {
          kind: PatKind::Binding(annotation, _, ident, _),
          ..
        }) => (
          ident,
          // Immutability here, will determine whether the variable is wrapped
          // in a mutable cell. We only say the binding is mutable for `Mutable`
          // not for `RefMut` because the latter happens in pattern matches and
          // we can't create/wrap new mutable cells patterns.
          matches!(annotation, hir::BindingAnnotation::Mutable),
        ),
        node => {
          xtor.unsupported(
            node.ident().map(|ident| ident.span).unwrap_or_default(),
            "Cannot extract complex pattern in binding (cannot recover from this)",
          );
          unreachable!()
        }
      };
      let id = xtor.register_hir(hir_id, ident.name.to_string());

      // Build a Variable node
      let tpe = xtor.extract_ty(self.tables.node_type(hir_id), &self.txtcx, ident.span);
      let tpe = if is_mutable {
        self.synth().mut_cell_type(tpe)
      } else {
        tpe
      };
      let flags = flags_opt
        .map(|flags| flags.to_stainless(f))
        .into_iter()
        .flatten()
        // Add @var flag if the param is mutable
        .chain(is_mutable.then(|| f.IsVar().into()))
        .collect();
      let var = f.Variable(id, tpe, flags);
      self.dcx.add_var(hir_id, var);
      var
    })
  }
}

/// DefContext tracks available bindings
#[derive(Clone, Debug, Default)]
pub(super) struct DefContext<'l> {
  vars: HashMap<HirId, &'l st::Variable<'l>>,
  params: Vec<&'l st::ValDef<'l>>,
}

impl<'l> DefContext<'l> {
  pub(super) fn params(&self) -> &[&'l st::ValDef<'l>] {
    &self.params[..]
  }

  pub(super) fn add_var(&mut self, hir_id: HirId, var: &'l st::Variable<'l>) {
    assert!(!self.vars.contains_key(&hir_id));
    self.vars.insert(hir_id, var);
  }

  /// Adds a parameter to the available bindings.
  pub(super) fn add_param(&mut self, vd: &'l st::ValDef<'l>) {
    assert!(!self.params.contains(&vd));
    self.params.push(vd);
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
