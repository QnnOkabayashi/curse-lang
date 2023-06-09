extern crate lalrpop;

fn main() {
    // lalrpop::process_root().unwrap();

    lalrpop::Configuration::new()
        // .log_debug()
        .emit_whitespace(false)
        .process_current_dir()
        .unwrap();
}
