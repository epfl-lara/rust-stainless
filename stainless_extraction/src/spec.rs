use std::collections::HashMap;
use std::convert::TryFrom;
use std::result::Result;

use super::*;
use crate::flags::{extract_flag, Flag};
use crate::ty::{all_generic_params_of, TyExtractionCtxt};

use rustc_ast::ast::Attribute;

use stainless_data::ast as st;

/// Types of spec functions (pre-, postconditions, ...) and some helping
/// implementations.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum SpecType {
  Pre,
  Post,
  Measure,
}

impl SpecType {
  /// Parses the spec type and the name of the function the spec belongs to from
  /// the name of the spec function.
  ///
  /// Spec functions are named according to the following format
  /// `__{spec_type}_{index}_{fn_name}`.
  ///
  pub fn parse_spec_type_fn_name(str: &str) -> Option<(SpecType, String)> {
    let parts: Vec<&str> = str.split('_').collect();
    match parts.as_slice() {
      ["", "", spec_type, index, ..] if index.chars().all(char::is_numeric) => {
        SpecType::try_from(*spec_type)
          .ok()
          .map(|sp| (sp, parts[4..].join("_")))
      }
      _ => None,
    }
  }
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

impl TryFrom<&str> for SpecType {
  type Error = ();

  fn try_from(name: &str) -> Result<Self, Self::Error> {
    match name {
      "pre" => Ok(SpecType::Pre),
      "post" => Ok(SpecType::Post),
      "measure" => Ok(SpecType::Measure),
      _ => Err(()),
    }
  }
}

impl<'a, 'l, 'tcx> BodyExtractor<'a, 'l, 'tcx> {
  pub fn extract_spec_closure(
    &mut self,
    hir_id: HirId,
    spec_type: SpecType,
    body_expr: st::Expr<'l>,
  ) -> st::Expr<'l> {
    // Spec functions are inner functions within the actual function being specified.
    // They take their own parameters, though those parameters are in a one-to-one correspondence
    // to the surrounding function's parameters. The analogous thing applies to type parameters.
    // Here we try to match all of them up and coerce the extraction to directly translate them to
    // the surrounding function's variables, instead of extracting new, unrelated identifiers.

    let def_id = self.tcx().hir().local_def_id(hir_id).to_def_id();
    let outer_fn_txtcx = &self.txtcx;
    let outer_fn_params = self.body_params();
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

    let (spec_expr, return_var) = self.base.enter_body(hir_id, txtcx, None, |bxtor| {
      // Correlate spec params with outer fn params
      let mut spec_params = bxtor.body.params.iter().collect::<Vec<_>>();

      let return_var = match spec_type {
        SpecType::Post => {
          // Remove ret parameter from the spec parameters
          let return_param = spec_params
            .pop()
            .expect("No return parameter on post spec function");

          let pattern_ty = bxtor.tables.pat_ty(return_param.pat);
          let return_tpe = bxtor
            .base
            .extract_ty(pattern_ty, &bxtor.txtcx, return_param.pat.span);
          let return_var = &*f.Variable(bxtor.base.fresh_id("ret".into()), return_tpe, vec![]);

          // Preregister `ret` binding with return_id, if any
          bxtor.dcx.add_var(return_param.pat.hir_id, return_var);
          Some(return_var)
        }
        _ => None,
      };

      assert_eq!(outer_fn_params.len(), spec_params.len());
      for (vd, sp) in outer_fn_params.iter().zip(spec_params) {
        bxtor.dcx.add_var(sp.pat.hir_id, vd.v);
      }

      // Pick up any additional local bindings
      bxtor.populate_def_context(&mut HashMap::new());

      // Extract the spec function's body
      let spec_expr = bxtor.hcx.mirror(&bxtor.body.value);
      (bxtor.extract_expr(spec_expr), return_var)
    });

    match spec_type {
      SpecType::Pre => f.Require(spec_expr, body_expr).into(),

      SpecType::Post => {
        let return_vd = f.ValDef(return_var.unwrap());
        f.Ensuring(body_expr, f.Lambda(vec![return_vd], spec_expr))
          .into()
      }

      SpecType::Measure => {
        // The measure function generated by the macro has a Unit return type to
        // deal with the unknown type of the measure. The expression has a
        // trailing ';' (UnitLiteral) to implement this. Here, we need to remove
        // the UnitLiteral and actually return the measure in the block
        // expression.
        if let st::Expr::Block(st::Block {
          exprs,
          last: st::Expr::UnitLiteral(st::UnitLiteral {}),
        }) = spec_expr
        {
          // Create a block that returns its last expression
          let (last, other_exprs) = exprs.split_last().expect("No measure provided.");

          f.Decreases(
            st::Expr::Block(f.Block(other_exprs.to_vec(), *last)),
            body_expr,
          )
          .into()
        } else {
          unreachable!("Ill-formed measure spec.")
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::spec::SpecType;

  #[test]
  fn test_parse_spec_type_fn_name() {
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__pre_1_asdf"),
      Some((SpecType::Pre, "asdf".to_string()))
    );
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__post_712341234_asdf"),
      Some((SpecType::Post, "asdf".to_string()))
    );
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__pre__asdf"),
      Some((SpecType::Pre, "asdf".to_string()))
    );
    assert_eq!(
      SpecType::parse_spec_type_fn_name("__measure_2_dummy_for_specs_2"),
      Some((SpecType::Measure, "dummy_for_specs_2".to_string()))
    );

    assert_eq!(
      SpecType::parse_spec_type_fn_name("__measure_bcdf_asdf"),
      None
    );
    assert_eq!(SpecType::parse_spec_type_fn_name("_pre_asdf"), None);
    assert_eq!(SpecType::parse_spec_type_fn_name("___pre_asdf"), None);
    assert_eq!(SpecType::parse_spec_type_fn_name("__pr_asdf"), None);
  }
}
