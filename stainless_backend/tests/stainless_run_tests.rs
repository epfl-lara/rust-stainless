extern crate stainless_backend;

use stainless_data::ast::Factory;

use stainless_backend::messages::{Report, Response};
use stainless_backend::{Backend, Config};

pub mod examples;

fn reponse_has_no_errors(response: Response) -> bool {
  match response.into_verification_report() {
    Some(Report::Verification { results, .. }) => {
      results.iter().all(|result| result.status.is_valid())
    }
    None => false,
  }
}

#[test]
fn test_one_query() {
  let f = Factory::new();
  let symbols = examples::identity_symbols(&f);
  let mut backend = Backend::create(Config::default()).unwrap();
  let response = backend.query_for_program(&symbols).unwrap();
  assert!(reponse_has_no_errors(response));
}

#[test]
fn test_many_queries() {
  let f = Factory::new();
  let symbols = examples::identity_symbols(&f);
  let mut backend = Backend::create(Config::default()).unwrap();
  for _ in 0..5 {
    let response = backend.query_for_program(&symbols).unwrap();
    assert!(reponse_has_no_errors(response));
  }
}
