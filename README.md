# Rust to Stainless extraction

## Setup and manual operation (**to be done**)

Clone and build stainless in `/basedir/stainless`

Clone this repo in `/basedir/stainless-rust-interop`

Make sure your `rustup` toolchain defaults to `nightly` (via `rustup default nightly`) (Most current nightly that should definitely work is `nightly-2020-03-12`).

Make sure you have installed the `rustc-dev` component (via `rustup toolchain install nightly --component rustc-dev`)

Build `stainless_driver` (via `cargo build` in the subfolder)

Set the environmental variable `RUST_STAINLESS_ARCH` (e.g. to `x86_64-unknown-linux-gnu`)

Run `./rustc_to_stainless program.rs`

Feed output to stainless
