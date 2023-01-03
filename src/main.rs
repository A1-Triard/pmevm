#![feature(alloc_error_handler)]
#![feature(const_maybe_uninit_as_mut_ptr)]
#![feature(const_mut_refs)]
#![feature(const_ptr_write)]
#![feature(const_trait_impl)]
#![feature(extern_types)]
#![feature(generic_arg_infer)]
#![feature(lang_items)]
#![feature(panic_info_message)]
#![feature(ptr_metadata)]
#![feature(raw_ref_op)]
#![feature(start)]
#![feature(unsize)]

#![deny(warnings)]
#![allow(clippy::assertions_on_constants)]
#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

#![windows_subsystem="console"]
#![no_std]
#![no_main]

extern crate alloc;
extern crate dos_errno_and_panic;
extern crate pc_atomics;
extern crate rlibc;

mod no_std {
    use composable_allocators::{AsGlobal};
    use composable_allocators::stacked::{self, Stacked};
    use core::arch::asm;
    use core::ffi::c_int;
    use core::fmt::{self, Formatter, Write};
    use core::mem::MaybeUninit;
    use exit_no_std::exit;
    use pc_ints::*;

    #[no_mangle]
    extern "C" fn _aulldiv() -> ! { panic!("10") }
    #[no_mangle]
    extern "C" fn _aullrem() -> ! { panic!("11") }
    #[no_mangle]
    extern "C" fn _chkstk() { }
    #[no_mangle]
    extern "C" fn _fltused() -> ! { panic!("13") }
    #[no_mangle]
    extern "C" fn strlen() -> ! { panic!("14") }

    const MEM_SIZE: usize = 500000;

    static mut MEM: [MaybeUninit<u8>; MEM_SIZE] = [MaybeUninit::uninit(); _];

    #[global_allocator]
    static ALLOCATOR: AsGlobal<Stacked<stacked::CtParams<MEM_SIZE>>> =
        AsGlobal(Stacked::from_static_array(unsafe { &mut MEM }));

    #[cfg(windows)]
    #[alloc_error_handler]
    fn rust_oom(_: core::alloc::Layout) -> ! {
        panic!("OOM")
    }
}
 
mod arraybox {
    use core::borrow::{Borrow, BorrowMut};
    use core::fmt::{self, Debug, Display, Formatter};
    use core::marker::{PhantomData, Unsize};
    use core::mem::{MaybeUninit, align_of, size_of};
    use core::ops::{Deref, DerefMut};
    use core::ptr::{self, Pointee, null};

    /// # Safety
    ///
    /// This trait cannot be implemented outside of this module.
    #[const_trait]
    pub unsafe trait Buf: Default {
        fn as_ptr(&self) -> *const u8;
        fn as_mut_ptr(&mut self) -> *mut u8;
        fn align() -> usize;
        fn len() -> usize;
    }

    pub struct BufFor<T>(MaybeUninit<T>);

    impl<T> const Default for BufFor<T> {
        fn default() -> BufFor<T> { BufFor(MaybeUninit::uninit()) }
    }

    unsafe impl<T> const Buf for BufFor<T> {
        fn as_ptr(&self) -> *const u8 { self.0.as_ptr() as _ }
        fn as_mut_ptr(&mut self) -> *mut u8 { self.0.as_mut_ptr() as _ }
        fn align() -> usize { align_of::<T>() }
        fn len() -> usize { size_of::<T>() }
    }

    pub struct ArrayBox<'a, T: ?Sized + 'a, B: Buf> {
        buf: B,
        metadata: <T as Pointee>::Metadata,
        phantom: PhantomData<&'a T>,
    }

    impl<'a, T: ?Sized + 'a, B: Buf> Drop for ArrayBox<'a, T, B> {
        fn drop(&mut self) {
            unsafe { ptr::drop_in_place(self.as_mut_ptr()) };
        }
    }

    impl<'a, T: ?Sized + 'a, B: Buf> ArrayBox<'a, T, B> {
        pub const fn new<S: Unsize<T>>(source: S) -> Self where B: ~const Buf + ~const Default {
            assert!(B::align() >= align_of::<S>());
            assert!(B::len() >= size_of::<S>());
            let source_null_ptr: *const T = null::<S>();
            let metadata = source_null_ptr.to_raw_parts().1;
            let mut res = ArrayBox { buf: B::default(), metadata, phantom: PhantomData };
            unsafe { ptr::write(res.buf.as_mut_ptr() as *mut S, source) };
            res
        }

        pub fn as_ptr(&self) -> *const T {
            let metadata = self.metadata;
            ptr::from_raw_parts(self.buf.as_ptr() as *const (), metadata)
        }

        pub fn as_mut_ptr(&mut self) -> *mut T {
            let metadata = self.metadata;
            ptr::from_raw_parts_mut(self.buf.as_mut_ptr() as *mut (), metadata)
        }
    }

    impl<'a, T: ?Sized + 'a, B: Buf> AsRef<T> for ArrayBox<'a, T, B> {
        fn as_ref(&self) -> &T {
            unsafe { &*self.as_ptr() }
        }
    }

    impl<'a, T: ?Sized + 'a, B: Buf> AsMut<T> for ArrayBox<'a, T, B> {
        fn as_mut(&mut self) -> &mut T {
            unsafe { &mut *self.as_mut_ptr() }
        }
    }
    impl<'a, T: ?Sized + 'a, B: Buf> Borrow<T> for ArrayBox<'a, T, B> {
        fn borrow(&self) -> &T { self.as_ref() }
    }

    impl<'a, T: ?Sized + 'a, B: Buf> BorrowMut<T> for ArrayBox<'a, T, B> {
        fn borrow_mut(&mut self) -> &mut T { self.as_mut() }
    }

    impl<'a, T: ?Sized + 'a, B: Buf> Deref for ArrayBox<'a, T, B> {
        type Target = T;

        fn deref(&self) -> &T { self.as_ref() }
    }

    impl<'a, T: ?Sized + 'a, B: Buf> DerefMut for ArrayBox<'a, T, B> {
        fn deref_mut(&mut self) -> &mut T { self.as_mut() }
    }

    impl<'a, T: Debug + ?Sized + 'a, B: Buf> Debug for ArrayBox<'a, T, B> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            self.as_ref().fmt(f)
        }
    }

    impl<'a, T: Display + ?Sized + 'a, B: Buf> Display for ArrayBox<'a, T, B> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            self.as_ref().fmt(f)
        }
    }
}

use arraybox::*;
use core::cmp::min;
use core::fmt::{self, Write};
use core::mem::{ManuallyDrop, replace};
use core::str::{self};
use pmevm_backend::{MONITOR, Computer, ComputerProgramExt, Keyboard, MachineCycles};
use pmevm_backend::Key as MKey;
use timer_no_std::{MonoClock, MonoTime};
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
        buf[.. s.len()].copy_from_slice(s.as_bytes());
        self.offset += s.len();
        Ok(())
    }
}

struct Colors {
    bg: Bg,
    box_: Fg,
    led: Fg,
    button: Bg,
    switch: Fg,
    info: Fg,
}

const COLOR: Colors = Colors {
    bg: Bg::Black,
    box_: Fg::Green,
    led: Fg::Red,
    button: Bg::Cyan,
    switch: Fg::Cyan,
    info: Fg::Cyan,
};

const GRAY: Colors = Colors {
    bg: Bg::None,
    box_: Fg::LightGray,
    led: Fg::LightGray,
    button: Bg::LightGray,
    switch: Fg::LightGray,
    info: Fg::LightGray,
};

struct Pmevm {
    colors: &'static Colors,
    cpu_frequency_100_k_hz: u16,
    fps: u16,
    computer: Computer,
    keyboard: Keyboard,
    cycle: Option<u8>,
    reset_pressed: bool,
    m_cycle_pressed: bool,
}

fn render_box(colors: &Colors, p: Point, rp: &mut RenderPort) {
    let bounds = Rect { tl: p, size: Vector { x: 70, y: 14 } };
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

fn render_leds(computer: &Computer, cycle: Option<u8>, colors: &Colors, mut p: Point, rp: &mut RenderPort) {
    for port in 0 .. 3 {
        let display = if port == 2 { cycle } else { None };
        render_led_line(display.unwrap_or_else(|| computer.peek_port(port)), colors, p, rp);
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
    rp.out(p.offset(Vector { x: 4, y: 1 }), colors.switch, colors.bg, "Auto");
    rp.out(p.offset(Vector { x: 4, y: 4 }), colors.switch, colors.bg, "Step");
}

fn render_reset(pressed: bool, colors: &Colors, p: Point, rp: &mut RenderPort) {
    rp.out(
        p,
        if pressed { colors.button.try_into().unwrap() } else { Fg::Black },
        if pressed { colors.bg } else { colors.button },
        "  Reset  "
    );
}

fn render_m_cycle(pressed: bool, colors: &Colors, p: Point, rp: &mut RenderPort) {
    rp.out(
        p,
        if pressed { colors.button.try_into().unwrap() } else { Fg::Black },
        if pressed { colors.bg } else { colors.button },
        " M.Cycle "
    );
}

fn render_key(keyboard: &Keyboard, key: MKey, text: &str, colors: &Colors, p: Point, rp: &mut RenderPort) {
    let pressed = keyboard.get(key);
    rp.out(
        p.offset(Vector { x: 0, y: -1 }),
        if pressed { Fg::Black } else { colors.button.try_into().unwrap() },
        colors.bg,
        "▄▄▄▄"
    );
    rp.out(
        p,
        if pressed { colors.button.try_into().unwrap() } else { Fg::Black },
        if pressed { Bg::Black } else { colors.button },
        text
    );
    rp.out(
        p.offset(Vector { x: 0, y: 1 }),
        if pressed { Fg::Black } else { colors.button.try_into().unwrap() },
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
    write!(buf, "{cpu_frequency_100_k_hz:02}").unwrap();
    let text = &mut buf.bytes[.. buf.offset + 1];
    text[text.len() - 1] = replace(&mut text[text.len() - 2], b'.');
    rp.out(
        p.offset(Vector { x: -(text.len() as u16 as i16), y: 0 }),
        colors.info, colors.bg,
        unsafe { str::from_utf8_unchecked(text) }
    );
    rp.out(p, colors.info, colors.bg, " MHz");
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
    let margin = Thickness::align(Vector { x: 70, y: 14 }, screen_size, HAlign::Center, VAlign::Center);
    let p = margin.shrink_rect(Rect { tl: Point { x: 0, y: 0 }, size: screen_size }).tl;
    render_box(pmevm.colors, p, rp);
    render_leds(&pmevm.computer, pmevm.cycle, pmevm.colors, p.offset(Vector { x: 63, y: 9 }), rp);
    render_switch(pmevm.cycle.is_none(), pmevm.colors, p.offset(Vector { x: 29, y: 4 }), rp);
    render_reset(pmevm.reset_pressed, pmevm.colors, p.offset(Vector { x: 29, y: 2 }), rp);
    render_m_cycle(pmevm.m_cycle_pressed, pmevm.colors, p.offset(Vector { x: 29, y: 11 }), rp);
    render_keys(&pmevm.keyboard, pmevm.colors, p.offset(Vector { x: 3, y: 2 }), rp);
    if !pmevm.computer.is_cpu_halted() && pmevm.cycle.is_none() {
        render_cpu_frequency(pmevm.cpu_frequency_100_k_hz, pmevm.colors, p.offset(Vector { x: 61, y: 11 }), rp);
    }
}

const FPS: u16 = 40;
const MAX_CPU_FREQUENCY_100_K_HZ: u16 = 10;
const MAX_TICKS_BALANCE: i32 = 5000 * MAX_CPU_FREQUENCY_100_K_HZ as u32 as i32;
const KEY_PRESS_MS: u8 = 100;

trait Mode {
    fn run(&mut self, pmevm: &mut Pmevm, cycles: u16);
    fn reset(&mut self, pmevm: &mut Pmevm);
}

#[repr(C)]
union ModeUnion {
    _auto: ManuallyDrop<AutoMode<'static>>,
    _step: ManuallyDrop<StepMode>,
}

type ModeBuf = BufFor<ModeUnion>;

struct AutoMode<'a> {
    cpu_time: MonoTime<'a>,
    ticks_balance: i32,
}

impl<'a> AutoMode<'a> {
    fn new(clock: &'a MonoClock) -> AutoMode<'a> {
        AutoMode {
            cpu_time: clock.time(),
            ticks_balance: 0,
        }
    }
}

impl<'a> Mode for AutoMode<'a> {
    fn run(&mut self, pmevm: &mut Pmevm, _: u16) {
        let cpu_ms = self.cpu_time.split_ms_u16().unwrap_or(u16::MAX);
        debug_assert!(self.ticks_balance <= 0 && self.ticks_balance >= -i32::from(u8::MAX));
        assert!(MAX_TICKS_BALANCE >= 0 && i32::MAX - MAX_TICKS_BALANCE > u8::MAX.into());
        let max_ticks_balance_delta = MAX_TICKS_BALANCE - self.ticks_balance;
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
        self.ticks_balance += ticks_balance_delta;
        while self.ticks_balance > 0 {
            pmevm.keyboard.step(&mut pmevm.computer);
            if let Some(cpu_ticks) = pmevm.computer.step_ticks() {
                self.ticks_balance -= i32::from(cpu_ticks);
            } else {
                self.ticks_balance = 0;
            };
        }
    }

    fn reset(&mut self, pmevm: &mut Pmevm) {
        pmevm.cycle = None;
    }
}

struct StepMode {
    cycles: MachineCycles,
    passed: u8,
}

impl StepMode {
    fn new() -> StepMode {
        StepMode {
            cycles: MachineCycles::new(),
            passed: 0,
        }
    }
}

impl Mode for StepMode {
    fn run(&mut self, pmevm: &mut Pmevm, cycles: u16) {
        for _ in 0 .. cycles {
            if self.cycles[self.passed as usize ..].is_empty() {
                pmevm.keyboard.step(&mut pmevm.computer);
                self.cycles = pmevm.computer.step_cycles();
                self.passed = 0;
            }
            pmevm.cycle = Some(self.cycles[self.passed as usize]);
            self.passed += 1;
        }
    }

    fn reset(&mut self, pmevm: &mut Pmevm) {
        self.cycles.clear();
        self.passed = 0;
        self.run(pmevm, 1);
    }
}

extern {
    type PEB;
}

#[allow(non_snake_case)]
#[no_mangle]
extern "stdcall" fn mainCRTStartup(_: *const PEB) -> u64 {
    let screen = unsafe { tuifw_screen::init() }.unwrap();
    let mut windows = WindowTree::new(screen, render);
    let mut pmevm = Pmevm {
        colors: &COLOR,
        computer: Computer::new(),
        keyboard: Keyboard::new(),
        cpu_frequency_100_k_hz: 0,
        fps: 0,
        cycle: None,
        reset_pressed: false,
        m_cycle_pressed: false,
    };
    pmevm.computer.poke_program(MONITOR.0);
    let clock = unsafe { MonoClock::new() };
    let mut time = clock.time();
    let mut keyboard_time = [None; 16];
    let mut reset_button_time = None;
    let mut m_cycle_button_time = None;
    let mut mode: ArrayBox<dyn Mode, ModeBuf> = ArrayBox::new(AutoMode::new(&clock));
    loop {
        for (key, key_time) in keyboard_time.iter_mut().enumerate() {
            let release = key_time
                .map_or(false, |x| clock.time().delta_ms_u8(x).map_or(true, |x| x >= KEY_PRESS_MS));
            if release {
                *key_time = None;
                pmevm.keyboard.set(MKey::n(key as u8).unwrap(), false);
            }
        }
        let release_reset_button = if let Some(reset_button_time) = reset_button_time {
            clock.time().delta_ms_u8(reset_button_time).map_or(true, |x| x >= KEY_PRESS_MS)
        } else {
            false
        };
        if release_reset_button {
            reset_button_time = None;
            pmevm.reset_pressed = false;
        }
        let release_m_cycle_button = if let Some(m_cycle_button_time) = m_cycle_button_time {
            clock.time().delta_ms_u8(m_cycle_button_time).map_or(true, |x| x >= KEY_PRESS_MS)
        } else {
            false
        };
        if release_m_cycle_button {
            m_cycle_button_time = None;
            pmevm.m_cycle_pressed = false;
        }
        let cycles = if let Some(event) = WindowTree::update(&mut windows, false, &mut pmevm).unwrap() {
            match event {
                Event::Key(_, Key::Escape) => break,
                Event::Key(_, Key::Backspace) => {
                    reset_button_time = Some(clock.time());
                    pmevm.reset_pressed = true;
                    pmevm.computer.reset();
                    mode.reset(&mut pmevm);
                },
                Event::Key(_, Key::Char('g')) => pmevm.colors = &GRAY,
                Event::Key(_, Key::Char('c')) => pmevm.colors = &COLOR,
                Event::Key(_, Key::Char('s')) => {
                    if pmevm.cycle.is_none() {
                        mode = ArrayBox::new(StepMode::new());
                        mode.reset(&mut pmevm);
                        debug_assert!(pmevm.cycle.is_some());
                    }
                },
                Event::Key(_, Key::Char('a')) => {
                    if pmevm.cycle.is_some() {
                        mode = ArrayBox::new(AutoMode::new(&clock));
                        mode.reset(&mut pmevm);
                        debug_assert!(pmevm.cycle.is_none());
                    }
                },
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
                keyboard_time[m_key as u8 as usize] = Some(clock.time());
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
            match event {
                Event::Key(n, Key::Char(' ')) => {
                    pmevm.m_cycle_pressed = true;
                    m_cycle_button_time = Some(clock.time());
                    n.get()
                },
                _ => 0
            }
        } else {
            0
        };
        windows.invalidate_screen();
        mode.run(&mut pmevm, cycles);
        let ms = time.split_ms_u16().unwrap_or(u16::MAX);
        assert!(FPS != 0 && u16::MAX / FPS > 8);
        pmevm.fps = (7 * pmevm.fps + min(FPS, 1000u16.checked_div(ms).unwrap_or(FPS)) + 4) / 8;
        clock.sleep_ms_u16((1000 / FPS).saturating_sub(ms));
    }
    0
}
