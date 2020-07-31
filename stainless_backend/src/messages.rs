use serde::Deserialize;

#[derive(Debug, Deserialize)]
#[serde(tag = "status")]
pub enum Response {
  Error { msg: String },
  Success { reports: Vec<Report> },
}

impl Response {
  #[allow(unreachable_patterns)]
  pub fn into_verification_report(self) -> Option<Report> {
    match self {
      Response::Success { reports } => reports.into_iter().find(|report| match report {
        Report::Verification { .. } => true,
        _ => false,
      }),
      _ => None,
    }
  }
}

#[derive(Debug, Deserialize)]
#[serde(tag = "component", content = "data")]
pub enum Report {
  Verification {
    results: Vec<VerificationResult>,
    sources: Vec<SourceIdentifier>,
  },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VerificationResult {
  pub id: SourceIdentifier,
  pub pos: SourcePosition,
  pub time: usize,
  pub status: VerificationStatus,
  pub solver_name: Option<String>,
  pub kind: String,
  pub derived_from: SourceIdentifier,
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub enum VerificationStatus {
  Valid {},
  ValidFromCache {},
  Inconclusive {},
  Invalid {},
}

impl VerificationStatus {
  pub fn is_valid(&self) -> bool {
    match self {
      VerificationStatus::Valid {} | VerificationStatus::ValidFromCache {} => true,
      _ => false,
    }
  }
}

#[derive(Debug, Deserialize)]
pub struct SourceIdentifier {
  pub name: String,
  pub gid: usize,
  pub id: usize,
}

#[derive(Debug, Deserialize)]
pub struct SourcePosition {}

#[test]
fn test_parse_report() {
  let msg = r#"{
    "status" : "Success",
    "reports" : [
      {
        "component" : "Verification",
        "data" : {
          "results" : [
            {
              "id" : {
                "name" : "get_foo",
                "gid" : 25,
                "id" : 0
              },
              "pos" : {
                "kind" : {
                  "Unknown" : {

                  }
                }
              },
              "time" : 0,
              "status" : {
                "ValidFromCache" : {

                }
              },
              "solverName" : null,
              "kind" : "match exhaustiveness",
              "derivedFrom" : {
                "name" : "get_foo",
                "gid" : 25,
                "id" : 0
              }
            }
          ],
          "sources" : [
            {
              "name" : "get_foo",
              "gid" : 25,
              "id" : 0
            },
            {
              "name" : "get_bar",
              "gid" : 52,
              "id" : 0
            }
          ]
        }
      }
    ]
  }
  "#;
  let _msg: Response = serde_json::from_str(msg).unwrap();
}

#[test]
fn test_parse_error() {
  let msg = r#"{
    "status" : "Error",
    "msg" : "Something went wrong"
  }"#;
  let _msg: Response = serde_json::from_str(msg).unwrap();
}
