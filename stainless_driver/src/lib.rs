#![feature(rustc_private)]

mod extraction;

extern crate rustc;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate syntax;

use rustc::session::config::ErrorOutputType;
use rustc::session::early_error;
use rustc_driver::{run_compiler, Callbacks, Compilation};
use rustc_hir::def_id::LOCAL_CRATE;
use rustc_interface::{interface, Queries};

pub fn run() -> Result<(), ()> {
    let mut callbacks = ExtractionCallbacks {};
    let file_loader = None;

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
    println!("(Running the cmpiler with args: {:?})", args);
    rustc_driver::install_ice_hook();
    rustc_driver::catch_fatal_errors(|| run_compiler(&args, &mut callbacks, file_loader, None))
        .map(|_| ())
        .map_err(|_| ())
}

struct ExtractionCallbacks {}

impl Callbacks for ExtractionCallbacks {
    fn config(&mut self, config: &mut interface::Config) {
        config.opts.debugging_opts.save_analysis = true;
    }

    fn after_expansion<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        _queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        Compilation::Continue
    }

    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        let crate_name = queries.crate_name().unwrap().peek().clone();

        let expanded_crate = &queries.expansion().unwrap().peek().0;
        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
            tcx.dep_graph.with_ignore(|| {
                println!("Analysing crate '{}'", crate_name);
                tcx.analysis(LOCAL_CRATE).unwrap();
                extraction::playground(tcx, crate_name, expanded_crate);
            });
        });

        Compilation::Stop
    }
}
