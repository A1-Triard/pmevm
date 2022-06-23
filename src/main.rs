#![feature(default_alloc_error_handler)]
#![feature(generic_arg_infer)]
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

use core::cmp::min;
use core::fmt::{self, Write};
use core::mem::replace;
use core::str::{self};
use pmevm_backend::{MONITOR, Computer, ComputerProgramExt, Keyboard};
use pmevm_backend::Key as MKey;
use timer_no_std::{MonoTime, sleep_ms_u16};
use tuifw_screen::{Attr, Color, Event, Key, Point, Range1d, Rect, Thickness, Vector};
use tuifw_window::{RenderPort, Window, WindowTree};

struct Buf<const LEN: usize> {
    bytes: [u8; LEN],
    offset: usize,
}

impl<const LEN: usize> Write for Buf<LEN> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let buf = &mut self.bytes[self.offset ..];
        if s.len() > buf.len() { return Err(fmt::Error); }
        (&mut buf[.. s.len()]).copy_from_slice(s.as_bytes());
        self.offset += s.len();
        Ok(())
    }
}

struct Pmevm {
    cpu_frequency_100_k_hz: u16,
    fps: u16,
    computer: Computer,
    keyboard: Keyboard,
}

fn render_box(p: Point, port: &mut RenderPort) {
    let bounds = Rect { tl: p, size: Vector { x: 71, y: 14 } };
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

fn render_led(on: bool, p: Point, rp: &mut RenderPort) {
    rp.out(p, Color::White, None, Attr::empty(), if on { "██" } else { "──" });
}

fn render_led_line(mut display: u8, mut p: Point, rp: &mut RenderPort) {
    for _ in 0 .. 8 {
        render_led(display & 0x01 != 0, p, rp);
        display = display >> 1;
        p = p.offset(Vector { x: -3, y: 0 });
    }
}

fn render_leds(computer: &Computer, mut p: Point, rp: &mut RenderPort) {
    for port in 0 .. 3 {
        render_led_line(computer.peek_port(port), p, rp);
        p = p.offset(Vector { x: 0, y: -3 });
    }
}

fn render_switch(on: bool, p: Point, rp: &mut RenderPort) {
    rp.out(p, Color::White, None, Attr::empty(), "┌─┐");
    rp.out(p.offset(Vector { x: 0, y: 1 }), Color::White, None, Attr::empty(),
        if on { "│█│" } else { "│ │" }
    );
    rp.out(p.offset(Vector { x: 0, y: 2 }), Color::White, None, Attr::empty(),
        if on { "│▀│" } else { "│ │" }
    );
    rp.out(p.offset(Vector { x: 0, y: 3 }), Color::White, None, Attr::empty(),
        if on { "│ │" } else { "│▄│" }
    );
    rp.out(p.offset(Vector { x: 0, y: 4 }), Color::White, None, Attr::empty(),
        if on { "│ │" } else { "│█│" }
    );
    rp.out(p.offset(Vector { x: 0, y: 5 }), Color::White, None, Attr::empty(), "└─┘");
    rp.out(p.offset(Vector { x: 4, y: 1 }), Color::White, None, Attr::empty(), "Auto");
    rp.out(p.offset(Vector { x: 4, y: 4 }), Color::White, None, Attr::empty(), "Step");
}

fn render_reset(p: Point, rp: &mut RenderPort) {
    rp.out(p, Color::Black, Some(Color::White), Attr::empty(), "  Reset  ");
}

fn render_m_cycle(p: Point, rp: &mut RenderPort) {
    rp.out(p, Color::Black, Some(Color::White), Attr::empty(), " M.Cycle ");
}

fn render_key(text: &str, p: Point, rp: &mut RenderPort) {
    rp.out(
        p.offset(Vector { x: 0, y: -1 }),
        Color::White, Some(Color::Black), Attr::empty(),
        "▄▄▄▄"
    );
    rp.out(p, Color::Black, Some(Color::White), Attr::empty(), text);
    rp.out(
        p.offset(Vector { x: 0, y: 1 }),
        Color::White, Some(Color::Black), Attr::empty(),
        "▀▀▀▀"
    );
}

fn render_keys(p: Point, rp: &mut RenderPort) {
    render_key("    ", p, rp);
    render_key(" HB ", p.offset(Vector { x: 0, y: 3 }), rp);
    render_key("  4 ", p.offset(Vector { x: 0, y: 6 }), rp);
    render_key("  0 ", p.offset(Vector { x: 0, y: 9 }), rp);
    render_key("    ", p.offset(Vector { x: 6, y: 0 }), rp);
    render_key(" LB ", p.offset(Vector { x: 6, y: 3 }), rp);
    render_key("  5 ", p.offset(Vector { x: 6, y: 6 }), rp);
    render_key("  1 ", p.offset(Vector { x: 6, y: 9 }), rp);
    render_key("    ", p.offset(Vector { x: 12, y: 0 }), rp);
    render_key("  E ", p.offset(Vector { x: 12, y: 3 }), rp);
    render_key("  6 ", p.offset(Vector { x: 12, y: 6 }), rp);
    render_key("  2 ", p.offset(Vector { x: 12, y: 9 }), rp);
    render_key("    ", p.offset(Vector { x: 18, y: 0 }), rp);
    render_key("  R ", p.offset(Vector { x: 18, y: 3 }), rp);
    render_key("  7 ", p.offset(Vector { x: 18, y: 6 }), rp);
    render_key("  3 ", p.offset(Vector { x: 18, y: 9 }), rp);
}

fn render_cpu_frequency(cpu_frequency_100_k_hz: u16, p: Point, rp: &mut RenderPort) {
    let mut buf = Buf { bytes: [0; 6], offset: 0 };
    write!(buf, "{:02}", cpu_frequency_100_k_hz).unwrap();
    let text = &mut buf.bytes[.. buf.offset + 1];
    text[text.len() - 1] = replace(&mut text[text.len() - 2], b'.');
    rp.out(
        p.offset(Vector { x: -(text.len() as u16 as i16), y: 0 }),
        Color::White, None, Attr::empty(),
        unsafe { str::from_utf8_unchecked(text) }
    );
    rp.out(p, Color::White, None, Attr::empty(), " MHz");
}

fn render(
    _tree: &WindowTree<Pmevm>,
    window: Option<Window>,
    rp: &mut RenderPort,
    pmevm: &mut Pmevm,
) {
    debug_assert!(window.is_none());
    rp.fill(|rp, p| rp.out(p, Color::White, None, Attr::empty(), " "));
    render_leds(&pmevm.computer, Point { x: 68, y: 14 }, rp);
    render_switch(true, Point { x: 34, y: 9 }, rp);
    render_reset(Point { x: 34, y: 7 }, rp);
    render_m_cycle(Point { x: 34, y: 16 }, rp);
    render_keys(Point { x: 7, y: 7 }, rp);
    render_box(Point { x: 4, y: 5 }, rp);
    if !pmevm.computer.is_cpu_halted() {
        render_cpu_frequency(pmevm.cpu_frequency_100_k_hz, Point { x: 66, y: 16 }, rp);
    }
}

const FPS: u16 = 40;
const MAX_CPU_FREQUENCY_100_K_HZ: u16 = 10;
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
    pmevm.computer.poke_program(MONITOR.0);
    let mut time = MonoTime::get();
    let mut cpu_time = time;
    let mut keyboard_time = [None; 16];
    let mut ticks_balance: i32 = 0;
    loop {
        for key in 0 .. 16 {
            let release = keyboard_time[key]
                .map_or(false, |x| MonoTime::get().delta_ms_u8(x).map_or(true, |x| x >= 50));
            if release {
                keyboard_time[key] = None;
                pmevm.keyboard.set(MKey::n(key as u8).unwrap(), false);
            }
        }
        if let Some(event) = WindowTree::update(&mut windows, false, &mut pmevm).unwrap() {
            if matches!(event, Event::Key(_, Key::Escape)) { break; }
            let m_key = match event {
                Event::Key(_, Key::Char('0')) => Some(MKey::K0),
                Event::Key(_, Key::Char('1')) => Some(MKey::K1),
                Event::Key(_, Key::Char('2')) => Some(MKey::K2),
                Event::Key(_, Key::Char('3')) => Some(MKey::K3),
                Event::Key(_, Key::Char('4')) => Some(MKey::K4),
                Event::Key(_, Key::Char('5')) => Some(MKey::K5),
                Event::Key(_, Key::Char('6')) => Some(MKey::K6),
                Event::Key(_, Key::Char('7')) => Some(MKey::K7),
                _ => None,
            };
            if let Some(m_key) = m_key {
                pmevm.keyboard.set(m_key, true);
                keyboard_time[m_key as u8 as usize] = Some(MonoTime::get());
            }
        }
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
            assert!(MAX_CPU_FREQUENCY_100_K_HZ != 0 && u16::MAX / MAX_CPU_FREQUENCY_100_K_HZ > 8);
            pmevm.cpu_frequency_100_k_hz = (7 * pmevm.cpu_frequency_100_k_hz + cpu_frequency_100_k_hz) / 8;
            max_ticks_balance_delta
        };
        ticks_balance += ticks_balance_delta;
        while ticks_balance > 0 {
            pmevm.keyboard.step(&mut pmevm.computer);
            if let Some(cpu_ticks) = pmevm.computer.step() {
                ticks_balance -= i32::from(cpu_ticks);
            } else {
                ticks_balance = 0;
            };
        }
        let ms = time.split_ms_u16().unwrap_or(u16::MAX);
        assert!(FPS != 0 && u16::MAX / FPS > 8);
        pmevm.fps = (7 * pmevm.fps + min(FPS, 1000u16.checked_div(ms).unwrap_or(FPS)) + 4) / 8;
        sleep_ms_u16((1000 / FPS).saturating_sub(ms));
    }
    0
}
