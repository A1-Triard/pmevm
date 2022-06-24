#![feature(default_alloc_error_handler)]
#![feature(generic_arg_infer)]
#![feature(lang_items)]
#![feature(start)]

#![deny(warnings)]
#![allow(clippy::assertions_on_constants)]

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
use tuifw_screen::{Bg, Event, Fg, HAlign, Key, Point};
use tuifw_screen::{Range1d, Rect, Thickness, VAlign, Vector};
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

struct Colors {
    bg: Bg,
    box_: Fg,
    led: Fg,
    key: Bg,
    button: Bg,
    button_text: Fg,
    switch: Fg,
    switch_text: Fg,
}

const COLOR: Colors = Colors {
    bg: Bg::Black,
    box_: Fg::Green,
    led: Fg::Red,
    key: Bg::Cyan,
    button: Bg::Blue,
    button_text: Fg::LightMagenta,
    switch: Fg::Blue,
    switch_text: Fg::LightBlue,
};

const GRAY: Colors = Colors {
    bg: Bg::None,
    box_: Fg::LightGray,
    led: Fg::LightGray,
    key: Bg::LightGray,
    button: Bg::LightGray,
    button_text: Fg::Black,
    switch: Fg::LightGray,
    switch_text: Fg::LightGray,
};

struct Pmevm {
    colors: &'static Colors,
    cpu_frequency_100_k_hz: u16,
    fps: u16,
    computer: Computer,
    keyboard: Keyboard,
}

fn render_box(colors: &Colors, p: Point, rp: &mut RenderPort) {
    let bounds = Rect { tl: p, size: Vector { x: 71, y: 14 } };
    let inner = Thickness::all(1).shrink_rect(bounds);
    for x in Range1d::new(inner.l(), inner.r()) {
        rp.out(Point { x, y: bounds.t() }, colors.box_, colors.bg, "═");
        rp.out(Point { x, y: bounds.b_inner() }, colors.box_, colors.bg, "═");
    }
    for y in Range1d::new(inner.t(), inner.b()) {
        rp.out(Point { x: bounds.l(), y }, colors.box_, colors.bg, "║");
        rp.out(Point { x: bounds.r_inner(), y }, colors.box_, colors.bg, "║");
    }
    rp.out(bounds.tl, colors.box_, colors.bg, "╔");
    rp.out(bounds.tr_inner(), colors.box_, colors.bg, "╗");
    rp.out(bounds.bl_inner(), colors.box_, colors.bg, "╚");
    rp.out(bounds.br_inner(), colors.box_, colors.bg, "╝");
    for x in Range1d::new(inner.l(), inner.r()) {
        for y in Range1d::new(inner.t(), inner.b()) {
            rp.out(Point { x, y }, Fg::Black, colors.bg, " ");
        }
    }
}

fn render_led(on: bool, colors: &Colors, p: Point, rp: &mut RenderPort) {
    rp.out(p, colors.led, colors.bg, if on { "██" } else { "──" });
}

fn render_led_line(mut display: u8, colors: &Colors, mut p: Point, rp: &mut RenderPort) {
    for _ in 0 .. 8 {
        render_led(display & 0x01 != 0, colors, p, rp);
        display >>= 1;
        p = p.offset(Vector { x: -3, y: 0 });
    }
}

fn render_leds(computer: &Computer, colors: &Colors, mut p: Point, rp: &mut RenderPort) {
    for port in 0 .. 3 {
        render_led_line(computer.peek_port(port), colors, p, rp);
        p = p.offset(Vector { x: 0, y: -3 });
    }
}

fn render_switch(on: bool, colors: &Colors, p: Point, rp: &mut RenderPort) {
    rp.out(p, colors.switch, colors.bg, "┌─┐");
    rp.out(p.offset(Vector { x: 0, y: 1 }), colors.switch, colors.bg, "│ │");
    rp.out(p.offset(Vector { x: 0, y: 2 }), colors.switch, colors.bg, "│ │");
    rp.out(p.offset(Vector { x: 0, y: 3 }), colors.switch, colors.bg, "│ │");
    rp.out(p.offset(Vector { x: 0, y: 4 }), colors.switch, colors.bg, "│ │");
    rp.out(p.offset(Vector { x: 0, y: 5 }), colors.switch, colors.bg, "└─┘");
    if on {
        rp.out(p.offset(Vector { x: 1, y: 1 }), colors.switch, colors.bg, "█");
        rp.out(p.offset(Vector { x: 1, y: 2 }), colors.switch, colors.bg, "▀");
    } else {
        rp.out(p.offset(Vector { x: 1, y: 3 }), colors.switch, colors.bg, "▄");
        rp.out(p.offset(Vector { x: 1, y: 4 }), colors.switch, colors.bg, "█");
    }
    rp.out(p.offset(Vector { x: 4, y: 1 }), colors.switch_text, colors.bg, "Auto");
    rp.out(p.offset(Vector { x: 4, y: 4 }), colors.switch_text, colors.bg, "Step");
}

fn render_reset(colors: &Colors, p: Point, rp: &mut RenderPort) {
    rp.out(p, colors.button_text, colors.button, "  Reset  ");
}

fn render_m_cycle(colors: &Colors, p: Point, rp: &mut RenderPort) {
    rp.out(p, colors.button_text, colors.button, " M.Cycle ");
}

fn render_key(keyboard: &Keyboard, key: MKey, text: &str, colors: &Colors, p: Point, rp: &mut RenderPort) {
    let pressed = keyboard.get(key);
    rp.out(
        p.offset(Vector { x: 0, y: -1 }),
        if pressed { Fg::Black } else { colors.key.try_into().unwrap() },
        colors.bg,
        "▄▄▄▄"
    );
    rp.out(
        p,
        if pressed { colors.key.try_into().unwrap() } else { Fg::Black },
        if pressed { Bg::Black } else { colors.key },
        text
    );
    rp.out(
        p.offset(Vector { x: 0, y: 1 }),
        if pressed { Fg::Black } else { colors.key.try_into().unwrap() },
        colors.bg,
        "▀▀▀▀"
    );
}

fn render_keys(keyboard: &Keyboard, colors: &Colors, p: Point, rp: &mut RenderPort) {
    render_key(keyboard, MKey::KC, "    ", colors, p, rp);
    render_key(keyboard, MKey::K8, " HB ", colors, p.offset(Vector { x: 0, y: 3 }), rp);
    render_key(keyboard, MKey::K4, "  4 ", colors, p.offset(Vector { x: 0, y: 6 }), rp);
    render_key(keyboard, MKey::K0, "  0 ", colors, p.offset(Vector { x: 0, y: 9 }), rp);
    render_key(keyboard, MKey::KD, "    ", colors, p.offset(Vector { x: 6, y: 0 }), rp);
    render_key(keyboard, MKey::K9, " LB ", colors, p.offset(Vector { x: 6, y: 3 }), rp);
    render_key(keyboard, MKey::K5, "  5 ", colors, p.offset(Vector { x: 6, y: 6 }), rp);
    render_key(keyboard, MKey::K1, "  1 ", colors, p.offset(Vector { x: 6, y: 9 }), rp);
    render_key(keyboard, MKey::KE, "    ", colors, p.offset(Vector { x: 12, y: 0 }), rp);
    render_key(keyboard, MKey::KA, "  E ", colors, p.offset(Vector { x: 12, y: 3 }), rp);
    render_key(keyboard, MKey::K6, "  6 ", colors, p.offset(Vector { x: 12, y: 6 }), rp);
    render_key(keyboard, MKey::K2, "  2 ", colors, p.offset(Vector { x: 12, y: 9 }), rp);
    render_key(keyboard, MKey::KF, "    ", colors, p.offset(Vector { x: 18, y: 0 }), rp);
    render_key(keyboard, MKey::KB, "  R ", colors, p.offset(Vector { x: 18, y: 3 }), rp);
    render_key(keyboard, MKey::K7, "  7 ", colors, p.offset(Vector { x: 18, y: 6 }), rp);
    render_key(keyboard, MKey::K3, "  3 ", colors, p.offset(Vector { x: 18, y: 9 }), rp);
}

fn render_cpu_frequency(cpu_frequency_100_k_hz: u16, colors: &Colors, p: Point, rp: &mut RenderPort) {
    let mut buf = Buf { bytes: [0; 6], offset: 0 };
    write!(buf, "{:02}", cpu_frequency_100_k_hz).unwrap();
    let text = &mut buf.bytes[.. buf.offset + 1];
    text[text.len() - 1] = replace(&mut text[text.len() - 2], b'.');
    rp.out(
        p.offset(Vector { x: -(text.len() as u16 as i16), y: 0 }),
        Fg::LightGray, colors.bg,
        unsafe { str::from_utf8_unchecked(text) }
    );
    rp.out(p, Fg::LightGray, colors.bg, " MHz");
}

fn render(
    tree: &WindowTree<Pmevm>,
    window: Option<Window>,
    rp: &mut RenderPort,
    pmevm: &mut Pmevm,
) {
    debug_assert!(window.is_none());
    rp.fill(|rp, p| rp.out(p, Fg::LightGray, Bg::None, " "));
    let screen_size = tree.screen_size();
    let margin = Thickness::align(Vector { x: 71, y: 14 }, screen_size, HAlign::Center, VAlign::Center);
    let p = margin.shrink_rect(Rect { tl: Point { x: 0, y: 0 }, size: screen_size }).tl;
    render_box(&pmevm.colors, p, rp);
    render_leds(&pmevm.computer, &pmevm.colors, p.offset(Vector { x: 64, y: 9 }), rp);
    render_switch(true, &pmevm.colors, p.offset(Vector { x: 30, y: 4 }), rp);
    render_reset(&pmevm.colors, p.offset(Vector { x: 30, y: 2 }), rp);
    render_m_cycle(&pmevm.colors, p.offset(Vector { x: 30, y: 11 }), rp);
    render_keys(&pmevm.keyboard, &pmevm.colors, p.offset(Vector { x: 3, y: 2 }), rp);
    if !pmevm.computer.is_cpu_halted() {
        render_cpu_frequency(pmevm.cpu_frequency_100_k_hz, &pmevm.colors, p.offset(Vector { x: 62, y: 11 }), rp);
    }
}

const FPS: u16 = 40;
const MAX_CPU_FREQUENCY_100_K_HZ: u16 = 10;
const MAX_TICKS_BALANCE: i32 = 5000 * MAX_CPU_FREQUENCY_100_K_HZ as u32 as i32;
const KEY_PRESS_MS: u8 = 100;

#[start]
fn main(_: isize, _: *const *const u8) -> isize {
    let screen = unsafe { tuifw_screen::init() }.unwrap();
    let mut windows = WindowTree::new(screen, render);
    let mut pmevm = Pmevm {
        colors: &COLOR,
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
        for (key, key_time) in keyboard_time.iter_mut().enumerate() {
            let release = key_time
                .map_or(false, |x| MonoTime::get().delta_ms_u8(x).map_or(true, |x| x >= KEY_PRESS_MS));
            if release {
                *key_time = None;
                pmevm.keyboard.set(MKey::n(key as u8).unwrap(), false);
            }
        }
        if let Some(event) = WindowTree::update(&mut windows, false, &mut pmevm).unwrap() {
            match event {
                Event::Key(_, Key::Escape) => break,
                Event::Key(_, Key::Backspace) => pmevm.computer.reset(),
                Event::Key(_, Key::Char('g')) => pmevm.colors = &GRAY,
                Event::Key(_, Key::Char('c')) => pmevm.colors = &COLOR,
                _ => { },
            }
            let m_key = match event {
                Event::Key(_, Key::Char('0')) => Some(MKey::K0),
                Event::Key(_, Key::Char('1')) => Some(MKey::K1),
                Event::Key(_, Key::Char('2')) => Some(MKey::K2),
                Event::Key(_, Key::Char('3')) => Some(MKey::K3),
                Event::Key(_, Key::Char('4')) => Some(MKey::K4),
                Event::Key(_, Key::Char('5')) => Some(MKey::K5),
                Event::Key(_, Key::Char('6')) => Some(MKey::K6),
                Event::Key(_, Key::Char('7')) => Some(MKey::K7),
                Event::Key(_, Key::Char('h')) => Some(MKey::K8),
                Event::Key(_, Key::Char('l')) => Some(MKey::K9),
                Event::Key(_, Key::Char('e')) => Some(MKey::KA),
                Event::Key(_, Key::Char('r')) => Some(MKey::KB),
                _ => None,
            };
            if let Some(m_key) = m_key {
                pmevm.keyboard.set(m_key, true);
                keyboard_time[m_key as u8 as usize] = Some(MonoTime::get());
            }
            let m_key = match event {
                Event::Key(_, Key::Char(')')) => Some(MKey::K0),
                Event::Key(_, Key::Char('!')) => Some(MKey::K1),
                Event::Key(_, Key::Char('@')) => Some(MKey::K2),
                Event::Key(_, Key::Char('#')) => Some(MKey::K3),
                Event::Key(_, Key::Char('$')) => Some(MKey::K4),
                Event::Key(_, Key::Char('%')) => Some(MKey::K5),
                Event::Key(_, Key::Char('^')) => Some(MKey::K6),
                Event::Key(_, Key::Char('&')) => Some(MKey::K7),
                Event::Key(_, Key::Char('H')) => Some(MKey::K8),
                Event::Key(_, Key::Char('L')) => Some(MKey::K9),
                Event::Key(_, Key::Char('E')) => Some(MKey::KA),
                Event::Key(_, Key::Char('R')) => Some(MKey::KB),
                _ => None,
            };
            if let Some(m_key) = m_key {
                pmevm.keyboard.set(m_key, !pmevm.keyboard.get(m_key));
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
            if let Some(cpu_ticks) = pmevm.computer.step_ticks() {
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
