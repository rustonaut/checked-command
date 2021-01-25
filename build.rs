use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    if env::var_os("CARGO_FEATURE_ENABLE_INTEGRATION_TESTS").is_some() {
        let compiler = env::var_os("RUSTC").unwrap();
        let target = env::var_os("TARGET").unwrap();

        let mut out_file = PathBuf::from(env::var_os("OUT_DIR").unwrap());
        out_file.push("feedback");

        let mut in_file = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap());
        in_file.push("test_helper_src");
        in_file.push("feedback_executable.rs");

        // this would be a good usage for CheckedCommand ;=)
        let exit_status = Command::new(compiler)
            .arg("--target")
            .arg(target)
            .arg("-o")
            .arg(out_file)
            .arg(in_file)
            .status()
            .expect("failed to execute rustc");

        if !exit_status.success() {
            panic!("rustc failed with exit status {:?}", exit_status.code())
        }
    }
}
