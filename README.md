# A Rust frontend for Stainless

This repository contains an experimental Rust frontend for the Stainless verifier.

## Overview

Our frontend is implemented as an alternative driver for rustc, which means that we run the standard Rust compiler and intercept its high-level IR after type-checking.
For a given compilation unit we produce a set of Stainless `ClassDef`s and `FunDef`s, and serialize them to a file which can then be verified independently using a new Stainless interface called `stainless-noxt` -- a "no-extraction" mode which merely deserializes the program representation from the previously generated file.
A Rust-version of Stainless data structures and associated serialization code is located in the `stainless_data` crate.
The actual code interfacing with rustc and performing program extraction is found in the `stainless_driver` crate.

## Installation

The following instructions assume that you have installed Rust using [rustup](https://github.com/rust-lang/rustup).
- Clone this repo in `/basedir/stainless-rust-interop`.
- Make sure your `rustup` toolchain defaults to `nightly` (via `rustup default nightly`; last tested with `nightly-2020-03-11`).
- Make sure you have the `rustc-dev` component installed (via `rustup toolchain install nightly --component rustc-dev`).
- Install `stainless_driver` (via `cargo install --path stainless_driver/`). This will build the `rustc_to_stainless` driver, which is essentially a modified version of `rustc`, and `cargo-stainless`, which provides a convenient way for invoking `rustc_to_stainless` from within Cargo project folders. Installation ensures that both of these binaries end up on your `PATH`.
- Clone and `sbt publishLocal` [inox with the modified serializer](https://github.com/epfl-lara/inox/tree/rust-interop) in `/basedir/inox`.
- Clone and build [stainless with the `stainless-noxt` frontend](https://github.com/epfl-lara/stainless/tree/rust-interop) in `/basedir/stainless` to verify the extracted programs.

## Usage

Assuming you have followed the above installation instructions, using `rustc_to_stainless` in basic Cargo projects is easy:
Navigate to a project folder (i.e., a folder containing a `Cargo.toml` file) and run `cargo stainless`.
This will invoke `rustc_to_stainless` for the last build target in your Cargo project with essentially the same configuration as `cargo build` would.
The frontend will produce some debug output, and, if successful, will write the extracted Stainless program to `./output.inoxser`.

You can then verify this program using Stainless and the `stainless-noxt` subproject.
To do so with a checked out version of the Stainless repo, run `sbt` in the root folder of the repo and consequently switch to the appropriate subproject using `project stainless-noxt`.
The actual verification can be started using `run /the/path/to/output.inoxser`.
For a slightly more practical setup, you can run Stainless in separate session and start it in watch mode using `run --watch /the/path/to/output.inoxser`.
This will automatically retrigger verification whenever `output.inoxser` is updated.

## What to expect

Note that the fragment of Rust currently supported is very limited. *TODO: Give some examples*

## Development

During development, it is useful to add `stainless_driver/target/debug` to `PATH`, so that `cargo-stainless` always invokes the most recently compiled version of `rustc_to_stainless`.
To interact directly with `rustc_to_stainless` (and avoid the indirection of Cargo), there exists an auxiliary script at `scripts/rustc_to_stainless` which injects some of the necessary flags.
The script can be used like the standard rustc binary, e.g., using `scripts/rustc_to_stainless --crate-type=lib examples/fact.rs` to extract the factorial function example.
