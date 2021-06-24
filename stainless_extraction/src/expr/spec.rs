use std::collections::HashMap;
use std::convert::TryFrom;
use std::result::Result;

use rustc_ast::ast::Attribute;

use stainless_data::ast as st;

use super::*;
use crate::flags::{extract_flag, Flag};
use crate::ty::{all_generic_params_of, TyExtractionCtxt};

/// Types of spec functions (pre-, postconditions, ...) and some helping
/// implementations.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum SpecType {
  Pre,
  Post,
  Measure,
}

impl TryFrom<Flag> for SpecType {
  type Error = ();

  fn try_from(flag: Flag) -> Result<Self, Self::Error> {
    match flag {
      Flag::Pre => Ok(SpecType::Pre),
      Flag::Post => Ok(SpecType::Post),
      Flag::Measure => Ok(SpecType::Measure),
      _ => Err(()),
    }
  }
}

impl TryFrom<&[Attribute]> for SpecType {
  type Error = ();

  fn try_from(attrs: &[Attribute]) -> Result<Self, Self::Error> {
    attrs
      .iter()
      .find_map(|a| {
        extract_flag(a)
          .ok()
          .and_then(|(flag, _)| SpecType::try_from(flag).ok())
      })
      .ok_or(())
  }
}

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub(super) fn extract_specs(
    &mut self,
    specs: &HashMap<SpecType, Vec<HirId>>,
    body_expr: st::Expr<'l>,
  ) -> st::Expr<'l> {
    let f = self.factory();

    // Wrap body with measure expression
    let body_expr = specs
      .get(&SpecType::Measure)
      .and_then(|m_exprs| {
        if m_exprs.len() > 1 {
          let span = self.tcx().hir().span(m_exprs[1]);
          self.tcx().sess.span_err(span, "Multiple measures.");
        }
        m_exprs.first()
      })
      .map(|&hid| {
        let expr = self.extract_spec_expr(hid, None);
        f.Decreases(expr, body_expr).into()
      })
      .unwrap_or(body_expr);

    // Wrap body with require/pre spec
    let body_expr = specs
      .get(&SpecType::Pre)
      .map(|pre_hids| {
        let exprs = pre_hids
          .iter()
          .map(|&hid| self.extract_spec_expr(hid, None))
          .collect();

        f.Require(f.make_and(exprs), body_expr).into()
      })
      .unwrap_or(body_expr);

    let return_var = &*f.Variable(self.base.fresh_id("ret".into()), self.return_tpe(), vec![]);
    let return_vd = f.ValDef(return_var);

    specs
      .get(&SpecType::Post)
      .map(move |post_hids| {
        let exprs = post_hids
          .iter()
          .map(|&hid| self.extract_spec_expr(hid, Some(return_var)))
          .collect();

        f.Ensuring(body_expr, f.Lambda(vec![return_vd], f.make_and(exprs)))
          .into()
      })
      .unwrap_or(body_expr)
  }

  /// Extract the body expression from a spec closure. This also replaces the
  /// parameters of the closure with the actual parameters of the outer function
  /// and the return value.
  fn extract_spec_expr(
    &mut self,
    hir_id: HirId,
    return_var: Option<&'l st::Variable<'l>>,
  ) -> st::Expr<'l> {
    // Specs are encoded as closure expressions within the actual (outer)
    // function being specified. They take their own parameters, though those
    // parameters are in a one-to-one correspondence to the surrounding
    // function's parameters. Here we try to match all of them up and coerce the
    // extraction to directly translate them to the surrounding function's
    // variables, instead of extracting new, unrelated identifiers.

    let def_id = self.base.hir_to_def_id(hir_id);
    let outer_fn_txtcx = &self.txtcx;
    let outer_fn_params = self.dcx.params();
    let f = self.factory();

    // Correlate type parameters
    // We need the generics of the parent (the outer function) to avoid some
    // special closure generics in the list.
    let parent_did = self.tcx().hir().get_parent_did(hir_id).to_def_id();
    let generics = self.tcx().generics_of(parent_did);
    let generic_params = all_generic_params_of(self.tcx(), parent_did);
    assert_eq!(
      generics.parent,
      self.tcx().generics_of(outer_fn_txtcx.def_id).parent
    );
    assert_eq!(generic_params.len(), outer_fn_txtcx.index_to_tparam.len());
    let index_to_tparam = generic_params
      .iter()
      .map(|param| {
        (
          param.index,
          outer_fn_txtcx.index_to_tparam[&param.index].clone(),
        )
      })
      .collect();

    let txtcx = TyExtractionCtxt {
      def_id,
      index_to_tparam,
    };

    let spec_expr = self
      .base
      .enter_body(hir_id, txtcx, self.current_class, |bxtor| {
        // Correlate spec params with outer fn params
        let mut spec_param_ids: Vec<HirId> =
          bxtor.body.params.iter().map(|p| p.pat.hir_id).collect();

        // If this is a post spec,
        if let Some(return_var) = return_var {
          // Remove the ret parameter from the spec parameters
          let return_param_id = spec_param_ids
            .pop()
            .expect("No return parameter on post spec function");

          // Preregister the ret binding with the return_id
          bxtor.dcx.add_var(return_param_id, return_var);
        }

        // Register all other bindings
        let (regular_params, ev_params) = outer_fn_params.split_at(spec_param_ids.len());
        for (vd, sid) in regular_params.iter().zip(spec_param_ids) {
          bxtor.dcx.add_var(sid, vd.v);
        }
        for e in ev_params {
          bxtor.dcx.add_param(e);
        }

        // Pick up any additional local bindings
        // (A spec neither has flags on the params, nor additional evidence params)
        bxtor.populate_def_context(&mut HashMap::new(), &[]);
        bxtor.extract_body_expr(def_id.expect_local())
      });

    // The measure closure generated by the macro has a Unit return type to deal
    // with the unknown type of the measure. The expression has a trailing ';'
    // (UnitLiteral) to implement this. Here, we need to remove the UnitLiteral
    // and actually return the measure in the block expression.
    if let st::Expr::Block(st::Block {
      exprs,
      last: st::Expr::UnitLiteral(st::UnitLiteral {}),
    }) = spec_expr
    {
      // Create a block that returns its last expression
      let (last, other_exprs) = exprs.split_last().expect("No measure provided.");
      st::Expr::Block(f.Block(other_exprs.to_vec(), *last))
    } else {
      spec_expr
    }
  }
}
