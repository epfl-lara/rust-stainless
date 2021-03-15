# A Rust frontend for Stainless

This repository contains an experimental Rust frontend for the Stainless verifier.

## Overview

Our frontend is implemented as an alternative driver for rustc, which means that we run the standard Rust compiler and intercept its high-level IR after type-checking.
For a given compilation unit we produce a set of Stainless `ClassDef`s and `FunDef`s, and serialize them to a file which can then be verified independently using a new Stainless interface called `stainless-noxt` -- a "no-extraction" mode which merely deserializes the program representation from the previously generated file.
A Rust version of Stainless data structures and associated serialization code is located in the `stainless_data` crate.
The actual code interfacing with rustc and performing program extraction is found in the `stainless_extraction` and `stainless_frontend` crates.

## Installation

The following instructions assume that you have installed Rust using [rustup](https://github.com/rust-lang/rustup).

- Clone this repo in `/basedir/rust-stainless`.
- Make sure your `rustup` toolchain within that directory is set to the currently supported nightly version (via `rustup override set $(cat rust-toolchain)` in `/basedir/rust-stainless`).
- Make sure you have the `rustc-dev` and `llvm-tools-preview` components installed (via `rustup component add rustc-dev llvm-tools-preview`).
- Install `stainless_frontend` (via `cargo install --path stainless_frontend/`). This will build the `rustc_to_stainless` driver, which is essentially a modified version of `rustc`, and `cargo-stainless`, which provides a convenient way of invoking `rustc_to_stainless` from within Cargo project folders. Installation ensures that both of these binaries end up on your `PATH`.

- Get a copy of the standalone version of `stainless-noxt` from the release of
Stainless that has the tag stated in [`.stainless-version`](.stainless-version).
For example, [this one](https://github.com/epfl-lara/stainless/releases/tag/noxt-0.8.0-41-g20d258d)
with the archive for [Linux](https://github.com/epfl-lara/stainless/releases/download/noxt-0.8.0-41-g20d258d/stainless-noxt-0.8.0-41-g20d258d-linux.zip)
or [macOS](https://github.com/epfl-lara/stainless/releases/download/noxt-0.8.0-41-g20d258d/stainless-noxt-0.8.0-41-g20d258d-mac.zip).
(This is based on forks of
[inox](https://github.com/epfl-lara/inox/tree/rust-interop) and
[stainless](https://github.com/epfl-lara/stainless/tree/rust-interop).)
  
- Extract it to `/basedir/stainless`.
- Make sure that your `STAINLESS_HOME` environment variable points to `/basedir/stainless`.

Now you should be good to go.

## Usage

Assuming you have followed the above installation instructions, using `rustc_to_stainless` in basic Cargo projects is easy:
Navigate to a project folder (i.e., a folder containing a `Cargo.toml` file), run `cargo build`, and, consequently, `cargo stainless`.
This will invoke `rustc_to_stainless` for the last build target in your Cargo project with essentially the same configuration as `cargo build` would.
The frontend will produce some debug output, and, if extraction is successful, send the program to stainless for verification.
Similarly to other cargo commands, you can also use `cargo stainless --example foo` to instead extract a specific example.

## What to expect

Note that the fragment of Rust currently supported is very limited. The test
cases give you a good impression of [what works](stainless_frontend/tests/pass)
and [what doesn't yet](stainless_frontend/tests/fail).

## Development

You can also verify the extracted programs directly using Stainless and the `stainless-noxt` subproject.
First, export the serialized stainless program using `cargo stainless --export output.inoxser`.
To then run verification on that file, navigate to your checked-out Stainless repo, run `sbt` in the root folder of the repo, and consequently switch to the appropriate subproject using `project stainless-noxt`.
The actual verification can be started using `run /the/path/to/output.inoxser`.

### Generating the Stainless ADT

The `rust-stainless` frontend transforms Rust programs to Stainless trees which
are fed to the Stainless backend. The definition of these trees is in found in
[`stainless_data`](stainless_data/src/ast/generated.rs) and is automatically generated from the 
Stainless repository. 

If Stainless gets new features or a fix, it is sometimes necessary to regenerate the AST in order to
stay compatible with the latest version of the Stainless backend. This is done from the `rust-interop` branch 
on [Stainless](https://github.com/epfl-lara/stainless/tree/rust-interop).

1. Checkout `epfl-lara/rust-interop`.
2. Rebase or change what is needed.
3. Run `sbt` and then `runMain stainless.utils.RustInteropGeneratorTool generated.rs`.
4. Move the newly `generated.rs` file to `stainless_data/src/ast/generated.rs`.
5. Rebuild and test the frontend to see whether everything still works and hope for the best.

## Contributors

- Georg Schmid ([@gsps](https://github.com/gsps))
- Romain Ruetschi ([@romac](https://github.com/romac))
- Yann Bolliger ([@yannbolliger](https://github.com/yannbolliger))
- Ivan Yurov ([@youroff](https://github.com/youroff) - semester project, Spring 2020)
