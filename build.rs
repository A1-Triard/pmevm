#![deny(warnings)]

fn main() {
    #[cfg(target_os="dos")]
    dos_cp_generator::build();
}
