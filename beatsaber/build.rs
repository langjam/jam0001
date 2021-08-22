macro_rules! reexport_env {
    ($var:literal) => {
        println!(
            concat!("cargo:rustc-env=", $var, "={}"),
            ::std::env::var($var).unwrap()
        )
    };
}

fn main() {
    reexport_env!("HOST");
}
