FROM debian:latest

RUN apt update && apt install -y curl default-jre-headless gcc unzip

# install rust toolchain
WORKDIR /rust-stainless
COPY rust-toolchain .
ENV RUSTUP_HOME=/usr/local/rustup CARGO_HOME=/usr/local/cargo
RUN curl -f https://sh.rustup.rs | \
	sh -s -- -y --default-toolchain $(cat rust-toolchain) --component rustc-dev llvm-tools-preview
ENV PATH=/usr/local/cargo/bin:$PATH

# install stainless
ENV STAINLESS_HOME=/stainless
COPY .stainless-version .
RUN curl -fL https://github.com/epfl-lara/stainless/releases/download/$(cat .stainless-version)/stainless-$(cat .stainless-version)-linux.zip -o stainless.zip
RUN unzip -d $STAINLESS_HOME stainless.zip

# install ourself
COPY . .
RUN cargo install --path stainless_frontend/

RUN mkdir /src
WORKDIR /src

CMD ["sh", "-ec", "cargo build; cargo stainless"]
