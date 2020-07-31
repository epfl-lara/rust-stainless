extern crate stainless_backend;

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
  let query_path = examples::identity_symbols();
  let mut backend = Backend::create(Config::default()).unwrap();
  let response = backend.query(query_path).unwrap();
  assert!(reponse_has_no_errors(response));
}

#[test]
fn test_many_queries() {
  let query_path = examples::identity_symbols();
  let query_path = query_path.path();
  let mut backend = Backend::create(Config::default()).unwrap();
  for _ in 0..5 {
    let response = backend.query(query_path).unwrap();
    assert!(reponse_has_no_errors(response));
  }
}
