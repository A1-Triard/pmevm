#![feature(default_alloc_error_handler)]
#![feature(lang_items)]
#![feature(start)]

#![deny(warnings)]

#![windows_subsystem="console"]
#![no_std]

#[cfg(windows)]
#[link(name="msvcrt")]
extern { }

extern crate alloc;

mod no_std {
    use composable_allocators::{AsGlobal, System};

    #[global_allocator]
    static ALLOCATOR: AsGlobal<System> = AsGlobal(System);

    #[cfg(not(feature="debug"))]
    #[panic_handler]
    fn panic(_panic: &core::panic::PanicInfo) -> ! {
        exit_no_std::exit(b'P')
    }

    #[cfg(all(windows, target_env="gnu", not(feature="debug")))]
    #[no_mangle]
    extern fn rust_eh_register_frames () { }

    #[cfg(all(windows, target_env="gnu", not(feature="debug")))]
    #[no_mangle]
    extern fn rust_eh_unregister_frames () { }
}

use pmevm_backend::{Computer, Keyboard};
use tuifw_screen::{Attr, Color, Point, Range1d, Rect, Thickness, Vector};
use tuifw_window::{RenderPort, Window, WindowTree};

struct Pmevm {
    computer: Computer,
    keyboard: Keyboard,
}

fn render_box(bounds: Rect, port: &mut RenderPort) {
    let inner = Thickness::all(1).shrink_rect(bounds);
    for x in Range1d::new(inner.l(), inner.r()) {
        port.out(Point { x, y: bounds.t() }, Color::White, None, Attr::empty(), "═");
        port.out(Point { x, y: bounds.b_inner() }, Color::White, None, Attr::empty(), "═");
    }
    for y in Range1d::new(inner.t(), inner.b()) {
        port.out(Point { x: bounds.l(), y }, Color::White, None, Attr::empty(), "║");
        port.out(Point { x: bounds.r_inner(), y }, Color::White, None, Attr::empty(), "║");
    }
    port.out(bounds.tl, Color::White, None, Attr::empty(), "╔");
    port.out(bounds.tr_inner(), Color::White, None, Attr::empty(), "╗");
    port.out(bounds.bl_inner(), Color::White, None, Attr::empty(), "╚");
    port.out(bounds.br_inner(), Color::White, None, Attr::empty(), "╝");
}

fn render(
    _tree: &WindowTree<Pmevm>,
    window: Option<Window>,
    port: &mut RenderPort,
    _pmevm: &mut Pmevm,
) {
    debug_assert!(window.is_none());
    port.fill(|port, p| port.out(p, Color::White, None, Attr::empty(), " "));
    render_box(Rect { tl: Point { x: 10, y: 10 }, size: Vector { x: 20, y: 10 } }, port);
}

#[start]
fn main(_: isize, _: *const *const u8) -> isize {
    let screen = unsafe { tuifw_screen::init() }.unwrap();
    let mut windows = WindowTree::new(screen, render);
    let mut pmevm = Pmevm { computer: Computer::new(), keyboard: Keyboard::new() };
    loop {
        pmevm.computer.step();
        pmevm.keyboard.step(&mut pmevm.computer);
        let _event = loop {
            if let Some(event) = WindowTree::update(&mut windows, false, &mut pmevm).unwrap() {
                break event;
            }
        };
    }
}
