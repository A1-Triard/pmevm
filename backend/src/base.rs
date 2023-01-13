use alloc::vec;
use alloc::vec::Vec;
use arrayvec::ArrayVec;
use core::mem::{replace, swap};
use educe::Educe;
use enumn::N;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Ord, PartialOrd, Hash)]
struct Psw(u8);

impl Psw {
    const fn new(code: u8) -> Psw {
        Psw(code | 0x02 & !0x08 & !0x20)
    }

    fn set_bit(&mut self, n: u8, v: bool) {
        let mask = 1 << n;
        let f = (v as u8).wrapping_neg();
        self.0 = ((self.0 ^ f) & !mask) ^ f;
    }

    fn zero(self) -> bool { (self.0 >> 6) & 0x01 != 0 }

    fn set_zero(&mut self, v: bool) { self.set_bit(6, v); }

    fn carry(self) -> bool { self.0 & 0x01 != 0 }

    fn set_carry(&mut self, v: bool) { self.set_bit(0, v); }

    fn parity(self) -> bool { (self.0 >> 2) & 0x01 != 0 }

    fn set_parity(&mut self, v: bool) { self.set_bit(2, v); }

    fn aux_carry(self) -> bool { (self.0 >> 4) & 0x01 != 0 }

    fn set_aux_carry(&mut self, v: bool) { self.set_bit(4, v); }

    fn sign(self) -> bool { self.0 >> 7 != 0 }

    fn set_sign(&mut self, v: bool) { self.set_bit(7, v); }

    fn check(&mut self, w: u8) {
        self.set_zero(w == 0);
        self.set_sign(w & 0xF0 != 0);
        self.set_parity(w & 0x01 == 0);
    }
}

impl From<Psw> for u8 {
    fn from(psw: Psw) -> u8 {
        psw.0
    }
}

#[derive(Debug, N, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u8)]
pub enum OpReg {
    A = 7,
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    M = 6
}

#[derive(Debug, N, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u8)]
pub enum OpExtReg {
    BC = 0,
    DE = 2
}

#[derive(Debug, N, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u8)]
pub enum OpRegPair {
    BC = 0,
    DE = 2,
    HL = 4,
    SP = 6,
}

#[derive(Debug, N, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u8)]
pub enum OpRegWord {
    BC = 0,
    DE = 2,
    HL = 4,
    PswA = 6,
}

#[derive(Debug, N, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u8)]
pub enum OpCond {
    NZ = 0,
    Z = 1,
    NC = 2,
    C = 3,
    PO = 4,
    PE = 5,
    P = 6,
    M = 7,
}

impl OpCond {
    fn test(self, psw: Psw) -> bool {
        match self {
            OpCond::NZ => !psw.zero(),
            OpCond::Z => psw.zero(),
            OpCond::NC => !psw.carry(),
            OpCond::C => psw.carry(),
            OpCond::PO => !psw.parity(),
            OpCond::PE => psw.parity(),
            OpCond::P => !psw.sign(),
            OpCond::M => psw.sign(),
        }
    }
}

#[derive(Debug, N, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u8)]
pub enum OpTriplet {
    _0 = 0,
    _1 = 1,
    _2 = 2,
    _3 = 3,
    _4 = 4,
    _5 = 5,
    _6 = 6,
    _7 = 7,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u8)]
pub enum Op {
    Inr(OpReg) = 0o004,
    Dcr(OpReg) = 0o005,
    Mov(OpReg, OpReg) = 0o100,
    Add(OpReg) = 0o200,
    Adc(OpReg) = 0o210,
    Sub(OpReg) = 0o220,
    Sbb(OpReg) = 0o230,
    Ana(OpReg) = 0o240,
    Xra(OpReg) = 0o250,
    Ora(OpReg) = 0o260,
    Cmp(OpReg) = 0o270,
    Inx(OpRegPair) = 0o003,
    Dcx(OpRegPair) = 0o013,
    Dad(OpRegPair) = 0o011,
    Pop(OpRegWord) = 0o301,
    Push(OpRegWord) = 0o305,
    Stax(OpExtReg) = 0o002,
    Ldax(OpExtReg) = 0o012,
    Rcc(OpCond) = 0o300,
    Ret = 0o311,
    Ret_ = 0o331,
    Rlc = 0o007,
    Rrc = 0o017,
    Ral = 0o027,
    Rar = 0o037,
    Xchg = 0o353,
    Xthl = 0o343,
    Sphl = 0o371,
    Pchl = 0o351,
    Nop(OpTriplet) = 0o000,
    Di = 0o363,
    Ei = 0o373,
    Daa = 0o047,
    Cma = 0o057,
    Stc = 0o067,
    Cmc = 0o077,
    Rst(OpTriplet) = 0o307,
    Adi = 0o306,
    Aci = 0o316,
    Sui = 0o326,
    Sbi = 0o336,
    Ani = 0o346,
    Xri = 0o356,
    Ori = 0o366,
    Cpi = 0o376,
    In = 0o333,
    Out = 0o323,
    Mvi(OpReg) = 0o006,
    Jcc(OpCond) = 0o302,
    Jmp = 0o303,
    Jmp_ = 0o313,
    Ccc(OpCond) = 0o304,
    Call(OpRegWord) = 0o315,
    Lxi(OpRegPair) = 0o001,
    Sta = 0o062,
    Lda = 0o072,
    Shld = 0o042,
    Lhld = 0o052,
}

impl Op {
    fn ticks(self, psw: Psw) -> u8 {
        match self {
            Op::Inr(OpReg::M) => 10,
            Op::Inr(_) => 5,
            Op::Dcr(OpReg::M) => 10,
            Op::Dcr(_) => 5,
            Op::Mov(OpReg::M, OpReg::M) => 7,
            Op::Mov(OpReg::M, _) => 7,
            Op::Mov(_, OpReg::M) => 7,
            Op::Mov(_, _) => 5,
            Op::Add(OpReg::M) => 7,
            Op::Add(_) => 4,
            Op::Adc(OpReg::M) => 7,
            Op::Adc(_) => 4,
            Op::Sub(OpReg::M) => 7,
            Op::Sub(_) => 4,
            Op::Sbb(OpReg::M) => 7,
            Op::Sbb(_) => 4,
            Op::Ana(OpReg::M) => 7,
            Op::Ana(_) => 4,
            Op::Xra(OpReg::M) => 7,
            Op::Xra(_) => 4,
            Op::Ora(OpReg::M) => 7,
            Op::Ora(_) => 4,
            Op::Cmp(OpReg::M) => 7,
            Op::Cmp(_) => 4,
            Op::Inx(_) | Op::Dcx(_) => 5,
            Op::Dad(_) => 10,
            Op::Pop(_) => 10,
            Op::Push(_) => 11,
            Op::Stax(_) | Op::Ldax(_) => 7,
            Op::Rcc(cond) => if cond.test(psw) { 11 } else { 5 },
            Op::Ret | Op::Ret_ => 10,
            Op::Rlc | Op::Rrc | Op::Ral | Op::Rar => 4,
            Op::Xchg => 4,
            Op::Xthl => 18,
            Op::Sphl | Op::Pchl => 5,
            Op::Nop(_) => 4,
            Op::Di | Op::Ei => 4,
            Op::Daa | Op::Cma => 4,
            Op::Stc | Op::Cmc => 4,
            Op::Rst(_) => 11,
            Op::Adi | Op::Aci | Op::Sui | Op::Sbi | Op::Ani | Op::Xri | Op::Ori | Op::Cpi => 7,
            Op::In | Op::Out => 10,
            Op::Mvi(OpReg::M) => 10,
            Op::Mvi(_) => 7,
            Op::Jcc(_) => 10,
            Op::Jmp | Op::Jmp_ => 10,
            Op::Ccc(cond) => if cond.test(psw) { 17 } else { 11 },
            Op::Call(_) => 17,
            Op::Lxi(_) => 10,
            Op::Sta | Op::Lda => 13,
            Op::Shld | Op::Lhld => 16,
        }
    }
}

impl From<Op> for u8 {
    fn from(op: Op) -> u8 {
        match op {
            Op::Shld => 0o042,
            Op::Lhld => 0o052,
            Op::Sta => 0o062,
            Op::Lda => 0o072,
            Op::Rlc => 0o007,
            Op::Rrc => 0o017,
            Op::Ral => 0o027,
            Op::Rar => 0o037,
            Op::Daa => 0o047,
            Op::Cma => 0o057,
            Op::Stc => 0o067,
            Op::Cmc => 0o077,
            Op::Ret => 0o311,
            Op::Ret_ => 0o331,
            Op::Pchl => 0o351,
            Op::Sphl => 0o371,
            Op::Jmp => 0o303,
            Op::Jmp_ => 0o313,
            Op::Out => 0o323,
            Op::In => 0o333,
            Op::Xthl => 0o343,
            Op::Xchg => 0o353,
            Op::Di => 0o363,
            Op::Ei => 0o373,
            Op::Adi => 0o306,
            Op::Aci => 0o316,
            Op::Sui => 0o326,
            Op::Sbi => 0o336,
            Op::Ani => 0o346,
            Op::Xri => 0o356,
            Op::Ori => 0o366,
            Op::Cpi => 0o376,
            Op::Mov(rd, rs) => 0o100 | ((rd as u8) << 3) | (rs as u8),
            Op::Add(r) => 0o200 | r as u8,
            Op::Adc(r) => 0o210 | r as u8,
            Op::Sub(r) => 0o220 | r as u8,
            Op::Sbb(r) => 0o230 | r as u8,
            Op::Ana(r) => 0o240 | r as u8,
            Op::Xra(r) => 0o250 | r as u8,
            Op::Ora(r) => 0o260 | r as u8,
            Op::Cmp(r) => 0o270 | r as u8,
            Op::Nop(a) => (a as u8) << 3,
            Op::Lxi(rp) => 0o001 | ((rp as u8) << 3),
            Op::Dad(rp) => 0o011 | ((rp as u8) << 3),
            Op::Stax(rp) => 0o002 | ((rp as u8) << 3),
            Op::Ldax(rp) => 0o012 | ((rp as u8) << 3),
            Op::Inx(rp) => 0o003 | ((rp as u8) << 3),
            Op::Dcx(rp) => 0o013 | ((rp as u8) << 3),
            Op::Inr(r) => 0o004 | ((r as u8) << 3),
            Op::Dcr(r) => 0o005 | ((r as u8) << 3),
            Op::Mvi(r) => 0o006 | ((r as u8) << 3),
            Op::Rcc(c) => 0o300 | ((c as u8) << 3),
            Op::Pop(rp) => 0o301 | ((rp as u8) << 3),
            Op::Jcc(c) => 0o302 | ((c as u8) << 3),
            Op::Ccc(c) => 0o304 | ((c as u8) << 3),
            Op::Push(rp) => 0o305 | ((rp as u8) << 3),
            Op::Call(rp) => 0o315 | ((rp as u8) << 3),
            Op::Rst(a) => 0o307 | ((a as u8) << 3),
        }
    }
}

impl From<u8> for Op {
    fn from(op: u8) -> Op {
        match op {
            0o042 => Op::Shld,
            0o052 => Op::Lhld,
            0o062 => Op::Sta,
            0o072 => Op::Lda,
            0o007 => Op::Rlc,
            0o017 => Op::Rrc,
            0o027 => Op::Ral,
            0o037 => Op::Rar,
            0o047 => Op::Daa,
            0o057 => Op::Cma,
            0o067 => Op::Stc,
            0o077 => Op::Cmc,
            0o311 => Op::Ret,
            0o331 => Op::Ret_,
            0o351 => Op::Pchl,
            0o371 => Op::Sphl,
            0o303 => Op::Jmp,
            0o313 => Op::Jmp_,
            0o323 => Op::Out,
            0o333 => Op::In,
            0o343 => Op::Xthl,
            0o353 => Op::Xchg,
            0o363 => Op::Di,
            0o373 => Op::Ei,
            0o306 => Op::Adi,
            0o316 => Op::Aci,
            0o326 => Op::Sui,
            0o336 => Op::Sbi,
            0o346 => Op::Ani,
            0o356 => Op::Xri,
            0o366 => Op::Ori,
            0o376 => Op::Cpi,
            x if x & 0o300 == 0o100 =>
                Op::Mov(OpReg::n((x >> 3) & 0x07).unwrap(), OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o200 => Op::Add(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o210 => Op::Adc(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o220 => Op::Sub(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o230 => Op::Sbb(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o240 => Op::Ana(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o250 => Op::Xra(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o260 => Op::Ora(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o370 == 0o270 => Op::Cmp(OpReg::n(x & 0x07).unwrap()),
            x if x & 0o307 == 0o000 => Op::Nop(OpTriplet::n((x >> 3) & 0x07).unwrap()),
            x if x & 0o317 == 0o001 => Op::Lxi(OpRegPair::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o317 == 0o011 => Op::Dad(OpRegPair::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o317 == 0o002 => Op::Stax(OpExtReg::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o317 == 0o012 => Op::Ldax(OpExtReg::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o317 == 0o003 => Op::Inx(OpRegPair::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o317 == 0o013 => Op::Dcx(OpRegPair::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o307 == 0o004 => Op::Inr(OpReg::n((x >> 3) & 0x07).unwrap()),
            x if x & 0o307 == 0o005 => Op::Dcr(OpReg::n((x >> 3) & 0x07).unwrap()),
            x if x & 0o307 == 0o006 => Op::Mvi(OpReg::n((x >> 3) & 0x07).unwrap()),
            x if x & 0o307 == 0o300 => Op::Rcc(OpCond::n((x >> 3) & 0x07).unwrap()),
            x if x & 0o317 == 0o301 => Op::Pop(OpRegWord::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o307 == 0o302 => Op::Jcc(OpCond::n((x >> 3) & 0x07).unwrap()),
            x if x & 0o307 == 0o304 => Op::Ccc(OpCond::n((x >> 3) & 0x07).unwrap()),
            x if x & 0o317 == 0o305 => Op::Push(OpRegWord::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o317 == 0o315 => Op::Call(OpRegWord::n((x >> 3) & 0x06).unwrap()),
            x if x & 0o307 == 0o307 => Op::Rst(OpTriplet::n((x >> 3) & 0x07).unwrap()),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
struct Memory(Vec<Vec<u8>>);

impl Memory {
    fn new() -> Self {
        Memory(vec![vec![0; 0x400]; 0x40])
    }

    fn get(&self, addr: u16) -> u8 {
        self.0[(addr >> 10) as usize][(addr & 0x03FF) as usize]
    }

    fn set(&mut self, addr: u16, b: u8) {
        self.0[(addr >> 10) as usize][(addr & 0x03FF) as usize] = b;
    }
}

#[derive(Debug, Clone)]
struct Cpu {
    halted: bool,
    interrupts_enabled: bool,
    psw: Psw,
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
}

impl Computer {
    fn reg_m(&self) -> u8 {
        self.mem.get(((self.cpu.h as u16) << 8) | (self.cpu.l as u16))
    }

    fn set_reg_m(&mut self, b: u8) {
        self.mem.set(((self.cpu.h as u16) << 8) | (self.cpu.l as u16), b);
    }

    fn reg(&self, r: OpReg) -> u8 {
        match r {
            OpReg::A => self.cpu.a,
            OpReg::B => self.cpu.b,
            OpReg::C => self.cpu.c,
            OpReg::D => self.cpu.d,
            OpReg::E => self.cpu.e,
            OpReg::H => self.cpu.h,
            OpReg::L => self.cpu.l,
            OpReg::M => self.reg_m()
        }
    }

    fn set_reg(&mut self, r: OpReg, b: u8) {
        match r {
            OpReg::A => self.cpu.a = b,
            OpReg::B => self.cpu.b = b,
            OpReg::C => self.cpu.c = b,
            OpReg::D => self.cpu.d = b,
            OpReg::E => self.cpu.e = b,
            OpReg::H => self.cpu.h = b,
            OpReg::L => self.cpu.l = b,
            OpReg::M => self.set_reg_m(b)
        }
    }

    fn ext_reg(&self, rp: OpExtReg) -> u16 {
        match rp {
            OpExtReg::BC => ((self.cpu.b as u16) << 8) | (self.cpu.c as u16),
            OpExtReg::DE => ((self.cpu.d as u16) << 8) | (self.cpu.e as u16),
        }
    }

    fn reg_pair(&self, rp: OpRegPair) -> u16 {
        match rp {
            OpRegPair::BC => ((self.cpu.b as u16) << 8) | (self.cpu.c as u16),
            OpRegPair::DE => ((self.cpu.d as u16) << 8) | (self.cpu.e as u16),
            OpRegPair::HL => ((self.cpu.h as u16) << 8) | (self.cpu.l as u16),
            OpRegPair::SP => self.cpu.sp,
        }
    }

    fn set_reg_pair(&mut self, rp: OpRegPair, w: u16) {
        let (h, l) = match rp {
            OpRegPair::BC => (&mut self.cpu.b, &mut self.cpu.c),
            OpRegPair::DE => (&mut self.cpu.d, &mut self.cpu.e),
            OpRegPair::HL => (&mut self.cpu.h, &mut self.cpu.l),
            OpRegPair::SP => { self.cpu.sp = w; return; },
        };
        *h = (w >> 8) as u8;
        *l = w as u8;
    }

    fn reg_word(&self, rp: OpRegWord) -> u16 {
        match rp {
            OpRegWord::BC => ((self.cpu.b as u16) << 8) | (self.cpu.c as u16),
            OpRegWord::DE => ((self.cpu.d as u16) << 8) | (self.cpu.e as u16),
            OpRegWord::HL => ((self.cpu.h as u16) << 8) | (self.cpu.l as u16),
            OpRegWord::PswA => ((u8::from(self.cpu.psw) as u16) << 8) | (self.cpu.a as u16),
        }
    }

    fn set_reg_word(&mut self, rp: OpRegWord, w: u16) {
        let (h, l) = match rp {
            OpRegWord::BC => (&mut self.cpu.b, &mut self.cpu.c),
            OpRegWord::DE => (&mut self.cpu.d, &mut self.cpu.e),
            OpRegWord::HL => (&mut self.cpu.h, &mut self.cpu.l),
            OpRegWord::PswA => {
                self.cpu.psw = Psw::new((w >> 8) as u8);
                self.cpu.a = w as u8;
                return;
            },
        };
        *h = (w >> 8) as u8;
        *l = w as u8;
    }
}

impl Cpu {
    const fn new() -> Cpu {
        Cpu {
            halted: false,
            interrupts_enabled: true,
            psw: Psw::new(0),
            a: 0, b: 0, c: 0, d: 0, e: 0, h: 0, l: 0,
            sp: 0, pc: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Port {
    in_: u8,
    out: u8,
}

#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct Computer {
    cpu: Cpu,
    #[educe(Debug(ignore))]
    mem: Memory,
    #[educe(Debug(ignore))]
    ports: [Port; (u8::MAX as u16 + 1) as usize],
}

impl Default for Computer {
    fn default() -> Computer { Computer::new() }
}

impl Computer {
    pub fn new() -> Computer {
        Computer {
            cpu: Cpu::new(),
            mem: Memory::new(),
            ports: [Port { in_: 0, out: 0 }; _],
        }
    }

    pub fn reset(&mut self) {
        self.cpu.pc = 0;
        self.cpu.halted = false;
        self.cpu.interrupts_enabled = true;
    }

    pub fn peek(&self, addr: u16) -> u8 {
        self.mem.get(addr)
    }

    pub fn poke(&mut self, addr: u16, b: u8) {
        self.mem.set(addr, b);
    }

    pub fn peek_port(&self, port: u8) -> u8 {
        self.ports[port as usize].out
    }

    pub fn poke_port(&mut self, port: u8, b: u8) {
        self.ports[port as usize].in_ = b;
    }

    pub fn is_cpu_halted(&self) -> bool {
        self.cpu.halted
    }

    pub fn are_interrupts_enabled(&self) -> bool {
        self.cpu.interrupts_enabled
    }

    fn load_byte(&self) -> u8 {
        self.mem.get(self.cpu.pc.wrapping_add(1))
    }

    fn load_word(&self) -> u16 {
        let l = self.mem.get(self.cpu.pc.wrapping_add(1));
        let h = self.mem.get(self.cpu.pc.wrapping_add(2));
        ((h as u16) << 8) | (l as u16)
    }

    pub fn step_cycles(&mut self) -> MachineCycles {
        let mut cycles = MachineCycles::new();
        if self.cpu.halted {
            cycles.push(0b10001010);
            return cycles;
        }
        let op = self.mem.get(self.cpu.pc);
        cycles.push(op);
        EXEC[usize::from(op)](op, self, Some(&mut cycles));
        cycles
    }

    pub fn step_ticks(&mut self) -> Option<u8> {
        if self.cpu.halted { return None; }
        let op = self.mem.get(self.cpu.pc);
        let ticks = Op::from(op).ticks(self.cpu.psw);
        EXEC[usize::from(op)](op, self, None);
        Some(ticks)
    }
}

pub type MachineCycles = ArrayVec<u8, 5>;

fn execute_add(computer: &mut Computer, a: u8, d: u8, c: bool) -> u8 {
    let aux = ((a & 0x0F) + (d & 0x0F) + c as u8) & 0xF0 != 0;
    let carry = ((a >> 4) + (d >> 4) + aux as u8) & 0xF0 != 0;
    computer.cpu.psw.set_aux_carry(aux);
    computer.cpu.psw.set_carry(carry);
    let a = a.wrapping_add(d).wrapping_add(c as u8);
    computer.cpu.psw.check(a);
    a
}

fn execute_sub(computer: &mut Computer, a: u8, d: u8, c: bool) -> u8 {
    let aux = (a & 0x0F) < (d & 0x0F) + c as u8;
    let carry = (a >> 4) < (d >> 4) + aux as u8;
    computer.cpu.psw.set_aux_carry(aux);
    computer.cpu.psw.set_carry(carry);
    let a = a.wrapping_sub(d).wrapping_sub(c as u8);
    computer.cpu.psw.check(a);
    a
}

#[allow(non_snake_case)]
fn exec_0X0_nop(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_0E1_lxi(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let rp = OpRegPair::n((op_code >> 3) & 0x06).unwrap();
    let d = computer.load_word();
    computer.set_reg_pair(rp, d);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(d as u8);
        cycles.push((d >> 8) as u8);
    }
}

#[allow(non_snake_case)]
fn exec_0O1_dad(op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let rp = OpRegPair::n((op_code >> 3) & 0x06).unwrap();
    let d = computer.reg_pair(rp);
    let hl = ((computer.cpu.h as u16) << 8) | (computer.cpu.l as u16);
    let sum = hl.wrapping_add(d);
    computer.cpu.psw.set_carry(sum < d || sum < hl);
    computer.cpu.h = (sum >> 8) as u8;
    computer.cpu.l = sum as u8;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_0F2_stax(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let rp = OpExtReg::n((op_code >> 3) & 0x06).unwrap();
    let addr = computer.ext_reg(rp);
    computer.mem.set(addr, computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.a);
    }
}

#[allow(non_snake_case)]
fn exec_0P2_ldax(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let rp = OpExtReg::n((op_code >> 3) & 0x06).unwrap();
    let addr = computer.ext_reg(rp);
    computer.cpu.a = computer.mem.get(addr);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.a);
    }
}

#[allow(non_snake_case)]
fn exec_042_shld(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let addr = computer.load_word();
    computer.mem.set(addr, computer.cpu.l);
    computer.mem.set(addr.wrapping_add(1), computer.cpu.h);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr as u8);
        cycles.push((addr >> 8) as u8);
        cycles.push(computer.cpu.l);
        cycles.push(computer.cpu.h);
    }
}

#[allow(non_snake_case)]
fn exec_052_lhld(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let addr = computer.load_word();
    computer.cpu.l = computer.mem.get(addr);
    computer.cpu.h = computer.mem.get(addr.wrapping_add(1));
    computer.cpu.pc = computer.cpu.pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr as u8);
        cycles.push((addr >> 8) as u8);
        cycles.push(computer.cpu.l);
        cycles.push(computer.cpu.h);
    }
}

#[allow(non_snake_case)]
fn exec_062_sta(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let addr = computer.load_word();
    computer.mem.set(addr, computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr as u8);
        cycles.push((addr >> 8) as u8);
        cycles.push(computer.cpu.a);
    }
}

#[allow(non_snake_case)]
fn exec_072_lda(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let addr = computer.load_word();
    computer.cpu.a = computer.mem.get(addr);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr as u8);
        cycles.push((addr >> 8) as u8);
        cycles.push(computer.cpu.a);
    }
}

#[allow(non_snake_case)]
fn exec_0E3_inx(op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let rp = OpRegPair::n((op_code >> 3) & 0x06).unwrap();
    let d = computer.reg_pair(rp);
    computer.set_reg_pair(rp, d.wrapping_add(1));
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_0O3_dcx(op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let rp = OpRegPair::n((op_code >> 3) & 0x06).unwrap();
    let d = computer.reg_pair(rp);
    computer.set_reg_pair(rp, d.wrapping_sub(1));
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_0X4_inr(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n((op_code >> 3) & 0x07).unwrap();
    let d = computer.reg(r);
    let a = execute_add(computer, d, 1, false);
    computer.set_reg(r, a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_0X5_dcr(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n((op_code >> 3) & 0x07).unwrap();
    let d = computer.reg(r);
    let a = execute_sub(computer, d, 1, false);
    computer.set_reg(r, a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_0X6_mvi(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n((op_code >> 3) & 0x07).unwrap();
    let d = computer.load_byte();
    computer.set_reg(r, d);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_007_rlc(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let carry = computer.cpu.a >> 7;
    computer.cpu.a = (computer.cpu.a << 1) | carry;
    computer.cpu.psw.set_carry(carry != 0);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_017_rrc(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let carry = computer.cpu.a << 7;
    computer.cpu.a = (computer.cpu.a >> 1) | carry;
    computer.cpu.psw.set_carry(carry != 0);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_027_ral(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let carry = computer.cpu.a >> 7;
    computer.cpu.a = (computer.cpu.a << 1) | computer.cpu.psw.carry() as u8;
    computer.cpu.psw.set_carry(carry != 0);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_037_rar(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let carry = computer.cpu.a << 7;
    computer.cpu.a = (computer.cpu.a >> 1) | ((computer.cpu.psw.carry() as u8) << 7);
    computer.cpu.psw.set_carry(carry != 0);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_047_daa(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let aux = computer.cpu.psw.aux_carry() as u8;
    let carry = (computer.cpu.psw.carry() as u8) << 4;
    let mut l = (computer.cpu.a & 0x0F) | (aux << 4);
    let mut h = ((computer.cpu.a >> 4) | carry).wrapping_sub(aux);
    let aux = l / 10 != 0;
    l %= 10;
    computer.cpu.psw.set_aux_carry(aux);
    h += aux as u8;
    let carry = h / 10 != 0;
    h %= 10;
    computer.cpu.psw.set_carry(carry);
    computer.cpu.a = (h << 4) | l;
    computer.cpu.psw.check(computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_057_cma(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.a = !computer.cpu.a;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_067_stc(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.psw.set_carry(true);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_077_cmc(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.psw.set_carry(!computer.cpu.psw.carry());
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_166_hlt(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.halted = true;
}

#[allow(non_snake_case)]
fn exec_1XX_mov(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let rd = OpReg::n((op_code >> 3) & 0x07).unwrap();
    let rs = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(rs);
    computer.set_reg(rd, d);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if rs == OpReg::M || rd == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_20X_add(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    let a = computer.cpu.a;
    computer.cpu.a = execute_add(computer, a, d, false);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_21X_adc(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    let a = computer.cpu.a;
    let c = computer.cpu.psw.carry();
    computer.cpu.a = execute_add(computer, a, d, c);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_22X_sub(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    let a = computer.cpu.a;
    let a = execute_sub(computer, a, d, false);
    computer.cpu.a = a;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_23X_sbb(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    let a = computer.cpu.a;
    let c = computer.cpu.psw.carry();
    let a = execute_sub(computer, a, d, c);
    computer.cpu.a = a;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_24X_ana(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    computer.cpu.psw.set_aux_carry(false);
    computer.cpu.psw.set_carry(false);
    computer.cpu.a &= d;
    computer.cpu.psw.check(computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_25X_xra(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    computer.cpu.psw.set_aux_carry(false);
    computer.cpu.psw.set_carry(false);
    computer.cpu.a ^= d;
    computer.cpu.psw.check(computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_26X_ora(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    computer.cpu.psw.set_aux_carry(false);
    computer.cpu.psw.set_carry(false);
    computer.cpu.a |= d;
    computer.cpu.psw.check(computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_27X_cmp(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let r = OpReg::n(op_code & 0x07).unwrap();
    let d = computer.reg(r);
    let a = computer.cpu.a;
    execute_sub(computer, a, d, false);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        if r == OpReg::M {
            cycles.push(d);
        }
    }
}

#[allow(non_snake_case)]
fn exec_3X0_rcc(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let cond = OpCond::n((op_code >> 3) & 0x07).unwrap();
    if cond.test(computer.cpu.psw) {
        exec_3P1_ret(op_code, computer, cycles);
    } else {
        computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    }
}

#[allow(non_snake_case)]
fn exec_3E1_pop(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let rp = OpRegWord::n((op_code >> 3) & 0x06).unwrap();
    let l = computer.mem.get(computer.cpu.sp);
    let h = computer.mem.get(computer.cpu.sp.wrapping_add(1));
    computer.set_reg_word(rp, ((h as u16) << 8) | (l as u16));
    computer.cpu.sp = computer.cpu.sp.wrapping_add(2);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(l);
        cycles.push(h);
    }
}

#[allow(non_snake_case)]
fn exec_3P1_ret(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let l = computer.mem.get(computer.cpu.sp);
    let h = computer.mem.get(computer.cpu.sp.wrapping_add(1));
    computer.cpu.pc = ((h as u16) << 8) | (l as u16);
    computer.cpu.sp = computer.cpu.sp.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(l);
        cycles.push(h);
    }
}

#[allow(non_snake_case)]
fn exec_351_pchl(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.pc = ((computer.cpu.h as u16) << 8) | (computer.cpu.l as u16);
}

#[allow(non_snake_case)]
fn exec_371_sphl(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.sp = ((computer.cpu.h as u16) << 8) | (computer.cpu.l as u16);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_3X2_jcc(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let cond = OpCond::n((op_code >> 3) & 0x07).unwrap();
    if cond.test(computer.cpu.psw) {
        exec_3Z3_jmp(op_code, computer, cycles);
    } else {
        computer.cpu.pc = computer.cpu.pc.wrapping_add(3);
    }
}

#[allow(non_snake_case)]
fn exec_3Z3_jmp(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let addr = computer.load_word();
    computer.cpu.pc = addr;
    if let Some(cycles) = cycles {
        cycles.push(addr as u8);
        cycles.push((addr >> 8) as u8);
    }
}

#[allow(non_snake_case)]
fn exec_323_out(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let port = computer.load_byte();
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    computer.ports[port as usize].out = computer.cpu.a;
    if let Some(cycles) = cycles {
        cycles.push(port);
        cycles.push(computer.cpu.a);
    }
}

#[allow(non_snake_case)]
fn exec_333_in(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let port = computer.load_byte();
    computer.cpu.a = computer.ports[port as usize].in_;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(port);
        cycles.push(computer.cpu.a);
    }
}

#[allow(non_snake_case)]
fn exec_343_xthl(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let l = computer.mem.get(computer.cpu.sp);
    let h = computer.mem.get(computer.cpu.sp.wrapping_add(1));
    let l = replace(&mut computer.cpu.l, l);
    let h = replace(&mut computer.cpu.h, h);
    computer.mem.set(computer.cpu.sp, l);
    computer.mem.set(computer.cpu.sp.wrapping_add(1), h);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.l);
        cycles.push(l);
        cycles.push(computer.cpu.h);
        cycles.push(h);
    }
}

#[allow(non_snake_case)]
fn exec_353_xchg(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    swap(&mut computer.cpu.h, &mut computer.cpu.d);
    swap(&mut computer.cpu.l, &mut computer.cpu.e);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_363_di(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.interrupts_enabled = false;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_373_ei(_op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    computer.cpu.interrupts_enabled = true;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
}

#[allow(non_snake_case)]
fn exec_3X4_ccc(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let cond = OpCond::n((op_code >> 3) & 0x07).unwrap();
    if cond.test(computer.cpu.psw) {
        exec_3O5_call(op_code, computer, cycles);
    } else {
        computer.cpu.pc = computer.cpu.pc.wrapping_add(3);
    }
}

#[allow(non_snake_case)]
fn exec_3E5_push(op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let rp = OpRegWord::n((op_code >> 3) & 0x06).unwrap();
    let w = computer.reg_word(rp);
    computer.mem.set(computer.cpu.sp.wrapping_sub(1), (w >> 8) as u8);
    computer.mem.set(computer.cpu.sp.wrapping_sub(2), w as u8);
    computer.cpu.sp = computer.cpu.sp.wrapping_sub(2);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(w as u8);
        cycles.push((w >> 8) as u8);
    }
}

#[allow(non_snake_case)]
fn exec_3O5_call(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let addr = computer.load_word();
    let ret = computer.cpu.pc.wrapping_add(3);
    computer.mem.set(computer.cpu.sp.wrapping_sub(2), ret as u8);
    computer.mem.set(computer.cpu.sp.wrapping_sub(1), (ret >> 8) as u8);
    computer.cpu.sp = computer.cpu.sp.wrapping_sub(2);
    computer.cpu.pc = addr;
    if let Some(cycles) = cycles {
        cycles.push(addr as u8);
        cycles.push((addr >> 8) as u8);
        cycles.push(ret as u8);
        cycles.push((ret >> 8) as u8);
    }
}

#[allow(non_snake_case)]
fn exec_306_adi(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    let a = computer.cpu.a;
    computer.cpu.a = execute_add(computer, a, d, false);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_316_aci(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    let a = computer.cpu.a;
    let c = computer.cpu.psw.carry();
    computer.cpu.a = execute_add(computer, a, d, c);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_326_sui(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    let a = computer.cpu.a;
    let a = execute_sub(computer, a, d, false);
    computer.cpu.a = a;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_336_sbi(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    let a = computer.cpu.a;
    let c = computer.cpu.psw.carry();
    let a = execute_sub(computer, a, d, c);
    computer.cpu.a = a;
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_346_ani(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    computer.cpu.psw.set_aux_carry(false);
    computer.cpu.psw.set_carry(false);
    computer.cpu.a &= d;
    computer.cpu.psw.check(computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_356_xri(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    computer.cpu.psw.set_aux_carry(false);
    computer.cpu.psw.set_carry(false);
    computer.cpu.a ^= d;
    computer.cpu.psw.check(computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_366_ori(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    computer.cpu.psw.set_aux_carry(false);
    computer.cpu.psw.set_carry(false);
    computer.cpu.a |= d;
    computer.cpu.psw.check(computer.cpu.a);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_376_cpi(_op_code: u8, computer: &mut Computer, cycles: Option<&mut MachineCycles>) {
    let d = computer.load_byte();
    let a = computer.cpu.a;
    execute_sub(computer, a, d, false);
    computer.cpu.pc = computer.cpu.pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
}

#[allow(non_snake_case)]
fn exec_3X7_rst(op_code: u8, computer: &mut Computer, _cycles: Option<&mut MachineCycles>) {
    let a = OpTriplet::n((op_code >> 3) & 0x07).unwrap();
    computer.cpu.pc = ((a as u8) << 3) as u16;
}

const EXEC: [fn(u8, &mut Computer, Option<&mut MachineCycles>); 256] = [
    exec_0X0_nop, exec_0E1_lxi,  exec_0F2_stax, exec_0E3_inx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_007_rlc,
    exec_0X0_nop, exec_0O1_dad,  exec_0P2_ldax, exec_0O3_dcx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_017_rrc,
    exec_0X0_nop, exec_0E1_lxi,  exec_0F2_stax, exec_0E3_inx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_027_ral,
    exec_0X0_nop, exec_0O1_dad,  exec_0P2_ldax, exec_0O3_dcx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_037_rar,
    exec_0X0_nop, exec_0E1_lxi,  exec_042_shld, exec_0E3_inx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_047_daa,
    exec_0X0_nop, exec_0O1_dad,  exec_052_lhld, exec_0O3_dcx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_057_cma,
    exec_0X0_nop, exec_0E1_lxi,  exec_062_sta,  exec_0E3_inx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_067_stc,
    exec_0X0_nop, exec_0O1_dad,  exec_072_lda,  exec_0O3_dcx,  exec_0X4_inr, exec_0X5_dcr,  exec_0X6_mvi, exec_077_cmc,

    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,
    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,
    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,
    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,
    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,
    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,
    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_166_hlt, exec_1XX_mov,
    exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,  exec_1XX_mov, exec_1XX_mov,

    exec_20X_add, exec_20X_add,  exec_20X_add,  exec_20X_add,  exec_20X_add, exec_20X_add,  exec_20X_add, exec_20X_add,
    exec_21X_adc, exec_21X_adc,  exec_21X_adc,  exec_21X_adc,  exec_21X_adc, exec_21X_adc,  exec_21X_adc, exec_21X_adc,
    exec_22X_sub, exec_22X_sub,  exec_22X_sub,  exec_22X_sub,  exec_22X_sub, exec_22X_sub,  exec_22X_sub, exec_22X_sub,
    exec_23X_sbb, exec_23X_sbb,  exec_23X_sbb,  exec_23X_sbb,  exec_23X_sbb, exec_23X_sbb,  exec_23X_sbb, exec_23X_sbb,
    exec_24X_ana, exec_24X_ana,  exec_24X_ana,  exec_24X_ana,  exec_24X_ana, exec_24X_ana,  exec_24X_ana, exec_24X_ana,
    exec_25X_xra, exec_25X_xra,  exec_25X_xra,  exec_25X_xra,  exec_25X_xra, exec_25X_xra,  exec_25X_xra, exec_25X_xra,
    exec_26X_ora, exec_26X_ora,  exec_26X_ora,  exec_26X_ora,  exec_26X_ora, exec_26X_ora,  exec_26X_ora, exec_26X_ora,
    exec_27X_cmp, exec_27X_cmp,  exec_27X_cmp,  exec_27X_cmp,  exec_27X_cmp, exec_27X_cmp,  exec_27X_cmp, exec_27X_cmp,

    exec_3X0_rcc, exec_3E1_pop,  exec_3X2_jcc,  exec_3Z3_jmp,  exec_3X4_ccc, exec_3E5_push, exec_306_adi, exec_3X7_rst,
    exec_3X0_rcc, exec_3P1_ret,  exec_3X2_jcc,  exec_3Z3_jmp,  exec_3X4_ccc, exec_3O5_call, exec_316_aci, exec_3X7_rst,
    exec_3X0_rcc, exec_3E1_pop,  exec_3X2_jcc,  exec_323_out,  exec_3X4_ccc, exec_3E5_push, exec_326_sui, exec_3X7_rst,
    exec_3X0_rcc, exec_3P1_ret,  exec_3X2_jcc,  exec_333_in,   exec_3X4_ccc, exec_3O5_call, exec_336_sbi, exec_3X7_rst,
    exec_3X0_rcc, exec_3E1_pop,  exec_3X2_jcc,  exec_343_xthl, exec_3X4_ccc, exec_3E5_push, exec_346_ani, exec_3X7_rst,
    exec_3X0_rcc, exec_351_pchl, exec_3X2_jcc,  exec_353_xchg, exec_3X4_ccc, exec_3O5_call, exec_356_xri, exec_3X7_rst,
    exec_3X0_rcc, exec_3E1_pop,  exec_3X2_jcc,  exec_363_di,   exec_3X4_ccc, exec_3E5_push, exec_366_ori, exec_3X7_rst,
    exec_3X0_rcc, exec_371_sphl, exec_3X2_jcc,  exec_373_ei,   exec_3X4_ccc, exec_3O5_call, exec_376_cpi, exec_3X7_rst,
];
