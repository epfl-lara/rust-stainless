fn main() -> Result<(), ()> {
  stainless_driver::run(|symbols| {
    let output_path = std::path::Path::new("./output.inoxser");
    stainless_driver::output_program(output_path, symbols);
  })
}
