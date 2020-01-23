# Rust to Stainless extraction

## Setup and manual operation (**to be done**)

Clone and build stainless in `/basedir/stainless`

Clone this repo in `/basedir/stainless-rust-interop`

Make sure your `rustup` toolchain defaults to `nightly-DD-MM-YYYY` (via `rustup default nightly-DD-MM-YYYY`) (**should store this in some file**) 

Make sure you have installed the `rustc-dev` component (via `rustup toolchain install nightly --profile default --component rustc-dev`)

Build `stainless_driver` (via `cargo build` in the subfolder)

Run `rustc_to_stainless program.rs`

Feed output to stainless
