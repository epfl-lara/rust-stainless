#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_ty;

use rustc_driver::{run_compiler, Callbacks, Compilation};
use rustc_hir::def_id::LOCAL_CRATE;
use rustc_interface::{interface, Queries};
use rustc_middle::ty::TyCtxt;

use stainless_data::ast as st;

pub fn run<E: FnOnce(TyCtxt<'_>, st::Symbols<'_>) + Send>(
  args: Vec<String>,
  on_extraction: E,
) -> Result<(), ()> {
  let mut callbacks = ExtractionCallbacks::new(on_extraction);
  let file_loader = None;
  rustc_driver::install_ice_hook();
  rustc_driver::catch_fatal_errors(|| run_compiler(&args, &mut callbacks, file_loader, None))
    .map(|_| ())
    .map_err(|_| ())
}

struct ExtractionCallbacks<E>
where
  E: FnOnce(TyCtxt<'_>, st::Symbols<'_>) + Send,
{
  on_extraction: Option<E>,
}

impl<E: FnOnce(TyCtxt<'_>, st::Symbols<'_>) + Send> ExtractionCallbacks<E> {
  fn new(on_extraction: E) -> Self {
    Self {
      on_extraction: Some(on_extraction),
    }
  }
}

impl<E: FnOnce(TyCtxt<'_>, st::Symbols<'_>) + Send> Callbacks for ExtractionCallbacks<E> {
  fn config(&mut self, config: &mut interface::Config) {
    config.opts.debugging_opts.save_analysis = true;
  }

  fn after_analysis<'tcx>(
    &mut self,
    _compiler: &interface::Compiler,
    queries: &'tcx Queries<'tcx>,
  ) -> Compilation {
    let crate_name: String = queries.crate_name().unwrap().peek().clone();

    queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
      tcx.dep_graph.with_ignore(|| {
        eprintln!("=== Analysing crate '{}' ===\n", crate_name);
        tcx.analysis(LOCAL_CRATE).unwrap();

        let factory = st::Factory::new();
        let symbols = stainless_extraction::extract_crate(tcx, &factory, crate_name);
        (self.on_extraction.take().expect("Already ran extraction"))(tcx, symbols);
      });
    });

    Compilation::Stop
  }
}
