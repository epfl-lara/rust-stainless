extern crate stainless_backend;

use stainless_data::ast::Factory;

use stainless_backend::messages::Report;
use stainless_backend::{verify_program, Backend, Config};

pub mod examples;

fn reponse_has_no_errors(report: Report) -> bool {
  match report {
    Report::Verification { results, .. } => results.iter().all(|result| result.status.is_valid()),
  }
}

#[test]
fn test_one_query() {
  let f = Factory::new();
  let symbols = examples::identity_symbols(&f);
  let report = verify_program(Config::default(), &symbols).unwrap();
  assert!(reponse_has_no_errors(report));
}

#[test]
fn test_many_queries() {
  let f = Factory::new();
  let symbols = examples::identity_symbols(&f);
  let mut backend = Backend::create(Config::default()).unwrap();
  for _ in 0..5 {
    let response = backend.query_for_program(&symbols).unwrap();
    if let Some(report) = response.into_verification_report() {
      assert!(reponse_has_no_errors(report));
    } else {
      assert!(false);
    }
  }
}
