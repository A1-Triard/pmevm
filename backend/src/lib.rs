#![feature(bigint_helper_methods)]
#![feature(const_trait_impl)]
#![feature(generic_arg_infer)]
#![feature(trait_alias)]

#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]

#![no_std]

pub mod i8080a;

extern crate alloc;

mod base;
pub use base::*;

mod program;
pub use program::*;

mod monitor;
pub use monitor::*;

mod keyboard;
pub use keyboard::*;
