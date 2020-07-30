fn main() -> Result<(), ()> {
  stainless_frontend::run(|symbols| {
    let output_path = std::path::Path::new("./output.inoxser");
    stainless_frontend::output_program(output_path, symbols);
  })
}
