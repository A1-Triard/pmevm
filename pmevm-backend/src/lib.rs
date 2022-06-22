#![feature(generic_arg_infer)]
#![feature(trait_alias)]

#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]

#![no_std]

mod base;
pub use base::*;

mod program;
pub use program::*;

mod monitor;
pub use monitor::*;

mod keyboard;
pub use keyboard::*;
