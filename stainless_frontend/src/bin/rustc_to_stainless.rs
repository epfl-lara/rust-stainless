#![feature(rustc_private)]
extern crate rustc_session;

use rustc_session::config::ErrorOutputType;
use rustc_session::early_error;

fn main() -> Result<(), ()> {
  let args = std::env::args_os()
    .enumerate()
    .map(|(i, arg)| {
      arg.into_string().unwrap_or_else(|arg| {
        early_error(
          ErrorOutputType::default(),
          &format!("Argument {} is not valid Unicode: {:?}", i, arg),
        )
      })
    })
    .collect::<Vec<_>>();

  stainless_frontend::run(args, |tcx, symbols| {
    tcx.sess.abort_if_errors();
    let output_path = std::path::Path::new("./output.inoxser");
    stainless_frontend::output_program(output_path, symbols);
  })
}
