#![feature(default_alloc_error_handler)]
#![feature(lang_items)]
#![feature(start)]

//#![deny(warnings)]

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

use alloc::string::ToString;
use core::cmp::min;
use pmevm_backend::{Computer, Keyboard};
use pmevm_backend::Key as MKey;
use timer_no_std::{MonoTime, sleep_ms_u16};
use tuifw_screen::{Attr, Color, Point, Range1d, Rect, Thickness, Vector};
use tuifw_window::{RenderPort, Window, WindowTree};

struct Pmevm {
    cpu_frequency_100_k_hz: u16,
    fps: u16,
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

fn render_key(key: MKey, text: &str, port: &mut RenderPort) {
    let row: i16 = (3 - (key as u8) / 4).into();
    let col: i16 = ((key as u8) % 4).into();
    let offset = Vector { x: 10 * col, y: 5 * row };
    render_box(Rect { tl: Point { x: 3, y: 3 }.offset(offset), size: Vector { x: 4, y: 3 } }, port);
}

fn render(
    _tree: &WindowTree<Pmevm>,
    window: Option<Window>,
    port: &mut RenderPort,
    pmevm: &mut Pmevm,
) {
    debug_assert!(window.is_none());
    port.fill(|port, p| port.out(p, Color::White, None, Attr::empty(), " "));
    render_box(Rect { tl: Point { x: 10, y: 10 }, size: Vector { x: 20, y: 10 } }, port);
    port.out(Point { x: 0, y: 0 }, Color::Blue, None, Attr::empty(), &pmevm.cpu_frequency_100_k_hz.to_string());
    port.out(Point { x: 0, y: 1 }, Color::Blue, None, Attr::empty(), &pmevm.fps.to_string());
    render_key(MKey::K0, "0", port);
    render_key(MKey::K1, "1", port);
}

const FPS: u16 = 40;
const MAX_CPU_FREQUENCY_100_K_HZ: u16 = 100;
const MAX_TICKS_BALANCE: i32 = 5000 * MAX_CPU_FREQUENCY_100_K_HZ as u32 as i32;

#[start]
fn main(_: isize, _: *const *const u8) -> isize {
    let screen = unsafe { tuifw_screen::init() }.unwrap();
    let mut windows = WindowTree::new(screen, render);
    let mut pmevm = Pmevm {
        computer: Computer::new(),
        keyboard: Keyboard::new(),
        cpu_frequency_100_k_hz: 0,
        fps: 0,
    };
    let mut time = MonoTime::get();
    let mut cpu_time = time;
    let mut ticks_balance: i32 = 0;
    loop {
        let _ = WindowTree::update(&mut windows, false, &mut pmevm).unwrap();
        windows.invalidate_screen();
        let cpu_ms = cpu_time.split_ms_u16().unwrap_or(u16::MAX);
        debug_assert!(ticks_balance <= 0 && ticks_balance >= -i32::from(u8::MAX));
        assert!(MAX_TICKS_BALANCE >= 0 && i32::MAX - MAX_TICKS_BALANCE > u8::MAX.into());
        let max_ticks_balance_delta = MAX_TICKS_BALANCE - ticks_balance;
        let ticks_balance_delta = (cpu_ms as u32) * ((100 * MAX_CPU_FREQUENCY_100_K_HZ) as u32);
        let ticks_balance_delta = if ticks_balance_delta <= max_ticks_balance_delta as u32 {
            pmevm.cpu_frequency_100_k_hz = MAX_CPU_FREQUENCY_100_K_HZ;
            ticks_balance_delta as i32
        } else {
            let cpu_frequency_100_k_hz = (max_ticks_balance_delta as u32 / cpu_ms as u32) as u16 / 100;
            assert!(u16::MAX / MAX_CPU_FREQUENCY_100_K_HZ > 256);
            pmevm.cpu_frequency_100_k_hz = (255 * pmevm.cpu_frequency_100_k_hz + cpu_frequency_100_k_hz) / 256;
            max_ticks_balance_delta
        };
        ticks_balance += ticks_balance_delta;
        while ticks_balance > 0 {
            pmevm.keyboard.step(&mut pmevm.computer);
            ticks_balance -= i32::from(pmevm.computer.step());
        }
        let ms = time.split_ms_u16().unwrap_or(u16::MAX);
        assert!(u16::MAX / FPS > 5);
        pmevm.fps = (4 * pmevm.fps + min(FPS, 1000 / ms)) / 5;
        sleep_ms_u16((1000 / FPS).saturating_sub(ms));
    }
}
