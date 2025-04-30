use std::path::Path;

const CODE_FILE: &str = "test.stak";

fn main() {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(CODE_FILE);
    let code = std::fs::read_to_string(CODE_FILE).unwrap();
}
