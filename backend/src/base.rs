use alloc::vec;
use alloc::vec::Vec;
use arrayvec::ArrayVec;
use core::mem::swap;
use core::ops::{Index, IndexMut};

#[derive(Clone)]
struct Memory(Vec<Vec<u8>>);

impl Memory {
    fn new() -> Self {
        Memory(vec![vec![0; 0x400]; 0x40])
    }
}

impl Index<u16> for Memory {
    type Output = u8;

    fn index(&self, addr: u16) -> &u8 {
        &self.0[usize::from(addr >> 10)][usize::from(addr & 0x03FF)]
    }
}

impl IndexMut<u16> for Memory {
    fn index_mut(&mut self, addr: u16) -> &mut u8 {
        &mut self.0[usize::from(addr >> 10)][usize::from(addr & 0x03FF)]
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(usize)]
enum RegW {
    Bc = 0,
    De = 1,
    Hl = 2,
    #[allow(dead_code)]
    Psw = 3,
    Pc = 4,
    Sp = 5,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(usize)]
enum RegB {
    B = 1,
    C = 0,
    D = 3,
    E = 2,
    H = 5,
    L = 4,
    A = 7,
    F = 6,
    PcH = 9,
    PcL = 8,
    SpH = 11,
    SpL = 10,
}

#[repr(C)]
union Regs {
    b: [u8; 12],
    w: [u16; 6],
}

impl Clone for Regs {
    fn clone(&self) -> Self {
        Regs { w: unsafe { self.w }.clone() }
    }
}

impl Regs {
    fn new() -> Regs {
        let this = Regs { b: [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0 ] };
        debug_assert!(this[RegB::F] == 0b00000010);
        this
    }
}

impl Index<RegB> for Regs {
    type Output = u8;

    fn index(&self, index: RegB) -> &u8 {
        unsafe { &self.b[index as usize] }
    }
}

impl IndexMut<RegB> for Regs {
    fn index_mut(&mut self, index: RegB) -> &mut u8 {
        unsafe { &mut self.b[index as usize] }
    }
}

impl Index<RegW> for Regs {
    type Output = u16;

    fn index(&self, index: RegW) -> &u16 {
        unsafe { &self.w[index as usize] }
    }
}

impl IndexMut<RegW> for Regs {
    fn index_mut(&mut self, index: RegW) -> &mut u16 {
        unsafe { &mut self.w[index as usize] }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u8)]
enum Flag {
    C = 0,
    P = 2,
    Ac = 4,
    Z = 6,
    S = 7
}

impl Flag {
    fn set(self, flags: &mut u8, value: bool) {
        *flags = *flags | ((value as u8) << (self as u8)) & !(((!value) as u8) << (self as u8));
    }
}

#[derive(Clone)]
struct Cpu {
    halted: bool,
    interrupts_enabled: bool,
    regs: Regs,
}

impl Cpu {
    fn new() -> Cpu {
        Cpu {
            halted: false,
            interrupts_enabled: true,
            regs: Regs::new()
        }
    }
}

#[derive(Clone, Copy)]
struct Port {
    in_: u8,
    out: u8,
}

#[derive(Clone)]
pub struct Computer {
    cpu: Cpu,
    mem: Memory,
    ports: [Port; 256],
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
        self.cpu.regs[RegW::Pc] = 0;
        self.cpu.halted = false;
        self.cpu.interrupts_enabled = true;
    }

    pub fn peek(&self, addr: u16) -> u8 {
        self.mem[addr]
    }

    pub fn poke(&mut self, addr: u16, b: u8) {
        self.mem[addr] = b;
    }

    pub fn peek_port(&self, port: u8) -> u8 {
        self.ports[usize::from(port)].out
    }

    pub fn poke_port(&mut self, port: u8, b: u8) {
        self.ports[usize::from(port)].in_ = b;
    }

    pub fn is_cpu_halted(&self) -> bool {
        self.cpu.halted
    }

    pub fn are_interrupts_enabled(&self) -> bool {
        self.cpu.interrupts_enabled
    }

    pub fn step_cycles(&mut self) -> MachineCycles {
        let mut cycles = MachineCycles::new();
        if self.cpu.halted {
            cycles.push(0xFF);
            return cycles;
        }
        let op = self.mem[self.cpu.regs[RegW::Pc]];
        cycles.push(op);
        EXEC[usize::from(op)](self, Some(&mut cycles));
        cycles
    }

    pub fn step_ticks(&mut self) -> Option<u8> {
        if self.cpu.halted { return None; }
        let op = self.mem[self.cpu.regs[RegW::Pc]];
        Some(EXEC[usize::from(op)](self, None))
    }
}

pub type MachineCycles = ArrayVec<u8, 5>;

const EXEC: [fn(&mut Computer, Option<&mut MachineCycles>) -> u8; 256] = [
    e_000_nop,     e_001_lxi_bc,  e_002_stax_bc, e_003_inx_bc,  e_004_inr_b,   e_005_dcr_b,   e_006_mvi_b,   e_007_rlc,
    e_000_nop,     e_011_dad_bc,  e_012_ldax_bc, e_013_dcx_bc,  e_014_inr_c,   e_015_dcr_c,   e_016_mvi_c,   e_017_rrc,
    e_000_nop,     e_021_lxi_de,  e_022_stax_de, e_023_inx_de,  e_024_inr_d,   e_025_dcr_d,   e_026_mvi_d,   e_027_ral,
    e_000_nop,     e_031_dad_de,  e_032_ldax_de, e_033_dcx_de,  e_034_inr_e,   e_035_dcr_e,   e_036_mvi_e,   e_037_rar,
    e_000_nop,     e_041_lxi_hl,  e_042_shld,    e_043_inx_hl,  e_044_inr_h,   e_045_dcr_h,   e_046_mvi_h,   e_047_daa,
    e_000_nop,     e_051_dad_hl,  e_052_lhld,    e_053_dcx_hl,  e_054_inr_l,   e_055_dcr_l,   e_056_mvi_l,   e_057_cma,
    e_000_nop,     e_061_lxi_sp,  e_062_sta,     e_063_inx_sp,  e_064_inr_m,   e_065_dcr_m,   e_066_mvi_m,   e_067_stc,
    e_000_nop,     e_071_dad_sp,  e_072_lda,     e_073_dcx_sp,  e_074_inr_a,   e_075_dcr_a,   e_076_mvi_a,   e_077_cmc,

    e_100_mov_nop, e_101_mov_b_c, e_102_mov_b_d, e_103_mov_b_e, e_104_mov_b_h, e_105_mov_b_l, e_106_mov_b_m, e_107_mov_b_a,
    e_110_mov_c_b, e_100_mov_nop, e_112_mov_c_d, e_113_mov_c_e, e_114_mov_c_h, e_115_mov_c_l, e_116_mov_c_m, e_117_mov_c_a,
    e_120_mov_d_b, e_121_mov_d_c, e_100_mov_nop, e_123_mov_d_e, e_124_mov_d_h, e_125_mov_d_l, e_126_mov_d_m, e_127_mov_d_a,
    e_130_mov_e_b, e_131_mov_e_c, e_132_mov_e_d, e_100_mov_nop, e_134_mov_e_h, e_135_mov_e_l, e_136_mov_e_m, e_137_mov_e_a,
    e_140_mov_h_b, e_141_mov_h_c, e_142_mov_h_d, e_143_mov_h_e, e_100_mov_nop, e_145_mov_h_l, e_146_mov_h_m, e_147_mov_h_a,
    e_150_mov_l_b, e_151_mov_l_c, e_152_mov_l_d, e_153_mov_l_e, e_154_mov_l_h, e_100_mov_nop, e_156_mov_l_m, e_157_mov_l_a,
    e_160_mov_m_b, e_161_mov_m_c, e_162_mov_m_d, e_163_mov_m_e, e_164_mov_m_h, e_165_mov_m_l, e_166_hlt,     e_167_mov_m_a,
    e_170_mov_a_b, e_171_mov_a_c, e_172_mov_a_d, e_173_mov_a_e, e_174_mov_a_h, e_175_mov_a_l, e_176_mov_a_m, e_100_mov_nop,

    e_200_add_b,   e_201_add_c,   e_202_add_d,   e_203_add_e,   e_204_add_h,   e_205_add_l,   e_206_add_m,   e_207_add_a,
    e_210_adc_b,   e_211_adc_c,   e_212_adc_d,   e_213_adc_e,   e_214_adc_h,   e_215_adc_l,   e_216_adc_m,   e_217_adc_a,
    e_220_sub_b,   e_221_sub_c,   e_222_sub_d,   e_223_sub_e,   e_224_sub_h,   e_225_sub_l,   e_226_sub_m,   e_227_sub_a,
    e_230_sbb_b,   e_231_sbb_c,   e_232_sbb_d,   e_233_sbb_e,   e_234_sbb_h,   e_235_sbb_l,   e_236_sbb_m,   e_237_sbb_a,
    e_240_ana_b,   e_241_ana_c,   e_242_ana_d,   e_243_ana_e,   e_244_ana_h,   e_245_ana_l,   e_246_ana_m,   e_247_ana_a,
    e_250_xra_b,   e_251_xra_c,   e_252_xra_d,   e_253_xra_e,   e_254_xra_h,   e_255_xra_l,   e_256_xra_m,   e_257_xra_a,
    e_260_ora_b,   e_261_ora_c,   e_262_ora_d,   e_263_ora_e,   e_264_ora_h,   e_265_ora_l,   e_266_ora_m,   e_247_ana_a,
    e_270_cmp_b,   e_271_cmp_c,   e_272_cmp_d,   e_273_cmp_e,   e_274_cmp_h,   e_275_cmp_l,   e_276_cmp_m,   e_277_cmp_a,

    e_300_rnz,     e_301_pop_bc,  e_302_jnz,     e_303_jmp,     e_304_cnz,     e_305_push_bc, e_306_adi,     e_307_rst_0,
    e_310_rz,      e_311_ret,     e_312_jz,      e_303_jmp,     e_314_cz,      e_315_call,    e_316_aci,     e_317_rst_1,
    e_320_rnc,     e_321_pop_de,  e_322_jnc,     e_323_out,     e_324_cnc,     e_325_push_de, e_326_sui,     e_327_rst_2,
    e_330_rc,      e_311_ret,     e_332_jc,      e_333_in,      e_334_cc,      e_315_call,    e_336_sbi,     e_337_rst_3,
    e_340_rpo,     e_341_pop_hl,  e_342_jpo,     e_343_xthl,    e_344_cpo,     e_345_push_hl, e_346_ani,     e_347_rst_4,
    e_350_rpe,     e_351_pchl,    e_352_jpe,     e_353_xchg,    e_354_cpe,     e_315_call,    e_356_xri,     e_357_rst_5,
    e_360_rp,      e_361_pop_af,  e_362_jp,      e_363_di,      e_364_cp,      e_365_push_af, e_366_ori,     e_367_rst_6,
    e_370_rm,      e_371_sphl,    e_372_jm,      e_373_ei,      e_374_cm,      e_315_call,    e_376_cpi,     e_377_rst_7,
];

fn e_000_nop(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    4
}

fn e_001_lxi_bc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::B] = computer.mem[pc.wrapping_add(2)];
    computer.cpu.regs[RegB::C] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::C]);
        cycles.push(computer.cpu.regs[RegB::B]);
    }
    10
}

fn e_021_lxi_de(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::D] = computer.mem[pc.wrapping_add(2)];
    computer.cpu.regs[RegB::E] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::E]);
        cycles.push(computer.cpu.regs[RegB::D]);
    }
    10
}

fn e_041_lxi_hl(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::H] = computer.mem[pc.wrapping_add(2)];
    computer.cpu.regs[RegB::L] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::L]);
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    10
}

fn e_061_lxi_sp(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::SpH] = computer.mem[pc.wrapping_add(2)];
    computer.cpu.regs[RegB::SpL] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::SpL]);
        cycles.push(computer.cpu.regs[RegB::SpH]);
    }
    10
}

fn e_011_dad_bc(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (hl, cf) = computer.cpu.regs[RegW::Hl].overflowing_add(computer.cpu.regs[RegW::Bc]);
    computer.cpu.regs[RegW::Hl] = hl;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    10
}

fn e_031_dad_de(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (hl, cf) = computer.cpu.regs[RegW::Hl].overflowing_add(computer.cpu.regs[RegW::De]);
    computer.cpu.regs[RegW::Hl] = hl;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    10
}

fn e_051_dad_hl(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (hl, cf) = computer.cpu.regs[RegW::Hl].overflowing_add(computer.cpu.regs[RegW::Hl]);
    computer.cpu.regs[RegW::Hl] = hl;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    10
}

fn e_071_dad_sp(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (hl, cf) = computer.cpu.regs[RegW::Hl].overflowing_add(computer.cpu.regs[RegW::Sp]);
    computer.cpu.regs[RegW::Hl] = hl;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    10
}

fn e_002_stax_bc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.mem[computer.cpu.regs[RegW::Bc]] = computer.cpu.regs[RegB::A];
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    7
}

fn e_012_ldax_bc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegB::A] = computer.mem[computer.cpu.regs[RegW::Bc]];
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    7
}

fn e_022_stax_de(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.mem[computer.cpu.regs[RegW::De]] = computer.cpu.regs[RegB::A];
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    7
}

fn e_032_ldax_de(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegB::A] = computer.mem[computer.cpu.regs[RegW::De]];
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    7
}

fn e_042_shld(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    let addr_h = computer.mem[pc.wrapping_add(2)];
    let addr_l = computer.mem[pc.wrapping_add(1)];
    let addr = ((addr_h as u16) << 8) | (addr_l as u16);
    computer.mem[addr] = computer.cpu.regs[RegB::L];
    computer.mem[addr.wrapping_add(1)] = computer.cpu.regs[RegB::H];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr_l);
        cycles.push(addr_h);
        cycles.push(computer.cpu.regs[RegB::L]);
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    16
}

fn e_052_lhld(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    let addr_h = computer.mem[pc.wrapping_add(2)];
    let addr_l = computer.mem[pc.wrapping_add(1)];
    let addr = ((addr_h as u16) << 8) | (addr_l as u16);
    computer.cpu.regs[RegB::L] = computer.mem[addr];
    computer.cpu.regs[RegB::H] = computer.mem[addr.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr_l);
        cycles.push(addr_h);
        cycles.push(computer.cpu.regs[RegB::L]);
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    16
}

fn e_062_sta(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    let addr_h = computer.mem[pc.wrapping_add(2)];
    let addr_l = computer.mem[pc.wrapping_add(1)];
    let addr = ((addr_h as u16) << 8) | (addr_l as u16);
    computer.mem[addr] = computer.cpu.regs[RegB::A];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr_l);
        cycles.push(addr_h);
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    13
}

fn e_072_lda(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    let addr_h = computer.mem[pc.wrapping_add(2)];
    let addr_l = computer.mem[pc.wrapping_add(1)];
    let addr = ((addr_h as u16) << 8) | (addr_l as u16);
    computer.cpu.regs[RegB::A] = computer.mem[addr];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    if let Some(cycles) = cycles {
        cycles.push(addr_l);
        cycles.push(addr_h);
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    13
}

fn e_003_inx_bc(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::Bc] = computer.cpu.regs[RegW::Bc].wrapping_add(1);
    5
}

fn e_023_inx_de(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::De] = computer.cpu.regs[RegW::De].wrapping_add(1);
    5
}

fn e_043_inx_hl(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::Hl] = computer.cpu.regs[RegW::Hl].wrapping_add(1);
    5
}

fn e_063_inx_sp(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::Sp] = computer.cpu.regs[RegW::Sp].wrapping_add(1);
    5
}

fn e_013_dcx_bc(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::Bc] = computer.cpu.regs[RegW::Bc].wrapping_sub(1);
    5
}

fn e_033_dcx_de(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::De] = computer.cpu.regs[RegW::De].wrapping_sub(1);
    5
}

fn e_053_dcx_hl(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::Hl] = computer.cpu.regs[RegW::Hl].wrapping_sub(1);
    5
}

fn e_073_dcx_sp(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::Sp] = computer.cpu.regs[RegW::Sp].wrapping_sub(1);
    5
}

fn e_004_inr_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::B].wrapping_add(1);
    computer.cpu.regs[RegB::B] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_005_dcr_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::B].wrapping_sub(1);
    computer.cpu.regs[RegB::B] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_014_inr_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::C].wrapping_add(1);
    computer.cpu.regs[RegB::C] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_015_dcr_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::C].wrapping_sub(1);
    computer.cpu.regs[RegB::C] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_024_inr_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::D].wrapping_add(1);
    computer.cpu.regs[RegB::D] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_025_dcr_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::D].wrapping_sub(1);
    computer.cpu.regs[RegB::D] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_034_inr_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::E].wrapping_add(1);
    computer.cpu.regs[RegB::E] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_035_dcr_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::E].wrapping_sub(1);
    computer.cpu.regs[RegB::E] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_044_inr_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::H].wrapping_add(1);
    computer.cpu.regs[RegB::H] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_045_dcr_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::H].wrapping_sub(1);
    computer.cpu.regs[RegB::H] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_054_inr_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::L].wrapping_add(1);
    computer.cpu.regs[RegB::L] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_055_dcr_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::L].wrapping_sub(1);
    computer.cpu.regs[RegB::L] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_064_inr_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let hl = computer.cpu.regs[RegW::Hl];
    let d = computer.mem[hl].wrapping_add(1);
    computer.mem[hl] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(d.wrapping_sub(1));
        cycles.push(d);
    }
    10
}

fn e_065_dcr_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let hl = computer.cpu.regs[RegW::Hl];
    let d = computer.mem[hl].wrapping_sub(1);
    computer.mem[hl] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(d.wrapping_add(1));
        cycles.push(d);
    }
    10
}

fn e_074_inr_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::A].wrapping_add(1);
    computer.cpu.regs[RegB::A] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_075_dcr_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let d = computer.cpu.regs[RegB::A].wrapping_sub(1);
    computer.cpu.regs[RegB::A] = d;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], d == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], d >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], d & 0x01 == 0);
    5
}

fn e_006_mvi_b(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::B] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::B]);
    }
    7
}

fn e_016_mvi_c(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::C] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::C]);
    }
    7
}

fn e_026_mvi_d(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::D] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::D]);
    }
    7
}

fn e_036_mvi_e(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::E] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::E]);
    }
    7
}

fn e_046_mvi_h(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::H] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    7
}

fn e_056_mvi_l(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::L] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::L]);
    }
    7
}

fn e_066_mvi_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    let hl = computer.cpu.regs[RegW::Hl];
    computer.mem[hl] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.mem[hl]);
        cycles.push(computer.mem[hl]);
    }
    10
}

fn e_076_mvi_a(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::A] = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    7
}

fn e_007_rlc(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A];
    let c = a >> 7;
    computer.cpu.regs[RegB::A] = (a << 1) | c;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], c != 0);
    4
}

fn e_017_rrc(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A];
    let c = a << 7;
    computer.cpu.regs[RegB::A] = (a >> 1) | c;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], c != 0);
    4
}

fn e_027_ral(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A];
    let c = a >> 7;
    computer.cpu.regs[RegB::A] = (a << 1) | computer.cpu.regs[RegB::F] & 0x01;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], c != 0);
    4
}

fn e_037_rar(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A];
    let c = a & 0x01;
    computer.cpu.regs[RegB::A] = (a >> 1) | (computer.cpu.regs[RegB::F] << 7);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], c != 0);
    4
}

fn e_047_daa(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    if computer.cpu.regs[RegB::A] & 0x0F >= 0x0A || computer.cpu.regs[RegB::F] & 0x10 != 0 {
        let (a, c) = computer.cpu.regs[RegB::A].overflowing_add(6);
        computer.cpu.regs[RegB::A] = a;
        computer.cpu.regs[RegB::F] |= 0x10 | c as u8;
    }
    if computer.cpu.regs[RegB::A] >= 0xA0 || computer.cpu.regs[RegB::F] & 0x01 != 0 {
        computer.cpu.regs[RegB::A] = computer.cpu.regs[RegB::A].wrapping_add(0x60);
        computer.cpu.regs[RegB::F] |= 0x01;
    }
    4
}

fn e_057_cma(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = !computer.cpu.regs[RegB::A];
    4
}

fn e_067_stc(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::F] |= 0x01;
    4
}

fn e_077_cmc(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::F] ^= 0x01;
    4
}

fn e_100_mov_nop(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    5
}

fn e_101_mov_b_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::B] = computer.cpu.regs[RegB::C];
    5
}

fn e_102_mov_b_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::B] = computer.cpu.regs[RegB::D];
    5
}

fn e_103_mov_b_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::B] = computer.cpu.regs[RegB::E];
    5
}

fn e_104_mov_b_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::B] = computer.cpu.regs[RegB::H];
    5
}

fn e_105_mov_b_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::B] = computer.cpu.regs[RegB::L];
    5
}

fn e_106_mov_b_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::B] = computer.mem[computer.cpu.regs[RegW::Hl]];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::B]);
    }
    7
}

fn e_107_mov_b_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::B] = computer.cpu.regs[RegB::A];
    5
}

fn e_110_mov_c_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::C] = computer.cpu.regs[RegB::B];
    5
}

fn e_112_mov_c_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::C] = computer.cpu.regs[RegB::D];
    5
}

fn e_113_mov_c_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::C] = computer.cpu.regs[RegB::E];
    5
}

fn e_114_mov_c_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::C] = computer.cpu.regs[RegB::H];
    5
}

fn e_115_mov_c_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::C] = computer.cpu.regs[RegB::L];
    5
}

fn e_116_mov_c_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::C] = computer.mem[computer.cpu.regs[RegW::Hl]];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::C]);
    }
    7
}

fn e_117_mov_c_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::C] = computer.cpu.regs[RegB::A];
    5
}

fn e_120_mov_d_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::D] = computer.cpu.regs[RegB::B];
    5
}

fn e_121_mov_d_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::D] = computer.cpu.regs[RegB::C];
    5
}

fn e_123_mov_d_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::D] = computer.cpu.regs[RegB::E];
    5
}

fn e_124_mov_d_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::D] = computer.cpu.regs[RegB::H];
    5
}

fn e_125_mov_d_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::D] = computer.cpu.regs[RegB::L];
    5
}

fn e_126_mov_d_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::D] = computer.mem[computer.cpu.regs[RegW::Hl]];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::D]);
    }
    7
}

fn e_127_mov_d_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::D] = computer.cpu.regs[RegB::A];
    5
}

fn e_130_mov_e_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::E] = computer.cpu.regs[RegB::B];
    5
}

fn e_131_mov_e_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::E] = computer.cpu.regs[RegB::C];
    5
}

fn e_132_mov_e_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::E] = computer.cpu.regs[RegB::D];
    5
}

fn e_134_mov_e_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::E] = computer.cpu.regs[RegB::H];
    5
}

fn e_135_mov_e_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::E] = computer.cpu.regs[RegB::L];
    5
}

fn e_136_mov_e_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::E] = computer.mem[computer.cpu.regs[RegW::Hl]];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::E]);
    }
    7
}

fn e_137_mov_e_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::E] = computer.cpu.regs[RegB::A];
    5
}

fn e_140_mov_h_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::H] = computer.cpu.regs[RegB::B];
    5
}

fn e_141_mov_h_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::H] = computer.cpu.regs[RegB::C];
    5
}

fn e_142_mov_h_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::H] = computer.cpu.regs[RegB::D];
    5
}

fn e_143_mov_h_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::H] = computer.cpu.regs[RegB::E];
    5
}

fn e_145_mov_h_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::H] = computer.cpu.regs[RegB::L];
    5
}

fn e_146_mov_h_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::H] = computer.mem[computer.cpu.regs[RegW::Hl]];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    7
}

fn e_147_mov_h_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::H] = computer.cpu.regs[RegB::A];
    5
}

fn e_150_mov_l_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::L] = computer.cpu.regs[RegB::B];
    5
}

fn e_151_mov_l_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::L] = computer.cpu.regs[RegB::C];
    5
}

fn e_152_mov_l_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::L] = computer.cpu.regs[RegB::D];
    5
}

fn e_153_mov_l_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::L] = computer.cpu.regs[RegB::E];
    5
}

fn e_154_mov_l_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::L] = computer.cpu.regs[RegB::H];
    5
}

fn e_156_mov_l_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::L] = computer.mem[computer.cpu.regs[RegW::Hl]];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::L]);
    }
    7
}

fn e_157_mov_l_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::L] = computer.cpu.regs[RegB::A];
    5
}

fn e_160_mov_m_b(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.mem[computer.cpu.regs[RegW::Hl]] = computer.cpu.regs[RegB::B];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::B]);
    }
    7
}

fn e_161_mov_m_c(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.mem[computer.cpu.regs[RegW::Hl]] = computer.cpu.regs[RegB::C];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::C]);
    }
    7
}

fn e_162_mov_m_d(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.mem[computer.cpu.regs[RegW::Hl]] = computer.cpu.regs[RegB::D];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::D]);
    }
    7
}

fn e_163_mov_m_e(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.mem[computer.cpu.regs[RegW::Hl]] = computer.cpu.regs[RegB::E];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::E]);
    }
    7
}

fn e_164_mov_m_h(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.mem[computer.cpu.regs[RegW::Hl]] = computer.cpu.regs[RegB::H];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    7
}

fn e_165_mov_m_l(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.mem[computer.cpu.regs[RegW::Hl]] = computer.cpu.regs[RegB::L];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::L]);
    }
    7
}

fn e_167_mov_m_a(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.mem[computer.cpu.regs[RegW::Hl]] = computer.cpu.regs[RegB::A];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    7
}

fn e_170_mov_a_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = computer.cpu.regs[RegB::B];
    5
}

fn e_171_mov_a_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = computer.cpu.regs[RegB::C];
    5
}

fn e_172_mov_a_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = computer.cpu.regs[RegB::D];
    5
}

fn e_173_mov_a_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = computer.cpu.regs[RegB::E];
    5
}

fn e_174_mov_a_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = computer.cpu.regs[RegB::H];
    5
}

fn e_175_mov_a_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = computer.cpu.regs[RegB::L];
    5
}

fn e_176_mov_a_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = computer.mem[computer.cpu.regs[RegW::Hl]];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    7
}

fn e_166_hlt(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.halted = true;
    7
}

fn e_200_add_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(computer.cpu.regs[RegB::B]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(computer.cpu.regs[RegB::B] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_201_add_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(computer.cpu.regs[RegB::C]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(computer.cpu.regs[RegB::C] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_202_add_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(computer.cpu.regs[RegB::D]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(computer.cpu.regs[RegB::D] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_203_add_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(computer.cpu.regs[RegB::E]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(computer.cpu.regs[RegB::E] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_204_add_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(computer.cpu.regs[RegB::H]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(computer.cpu.regs[RegB::H] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_205_add_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(computer.cpu.regs[RegB::L]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(computer.cpu.regs[RegB::L] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_206_add_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(m);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(m << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_207_add_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(computer.cpu.regs[RegB::A]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(computer.cpu.regs[RegB::A] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_210_adc_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(computer.cpu.regs[RegB::B], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (computer.cpu.regs[RegB::B] & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_211_adc_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(computer.cpu.regs[RegB::C], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (computer.cpu.regs[RegB::C] & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_212_adc_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(computer.cpu.regs[RegB::D], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (computer.cpu.regs[RegB::D] & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_213_adc_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(computer.cpu.regs[RegB::E], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (computer.cpu.regs[RegB::E] & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_214_adc_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(computer.cpu.regs[RegB::H], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (computer.cpu.regs[RegB::H] & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_215_adc_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(computer.cpu.regs[RegB::L], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (computer.cpu.regs[RegB::L] & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_216_adc_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(m, c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (m & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_217_adc_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(computer.cpu.regs[RegB::A], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (computer.cpu.regs[RegB::A] & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_220_sub_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::B]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::B] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_221_sub_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::C]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::C] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_222_sub_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::D]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::D] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_223_sub_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::E]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::E] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_224_sub_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::H]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::H] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_225_sub_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::L]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::L] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_226_sub_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(m);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(m << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_227_sub_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::A]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::A] << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_230_sbb_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(computer.cpu.regs[RegB::B], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (computer.cpu.regs[RegB::B] & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_231_sbb_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(computer.cpu.regs[RegB::C], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (computer.cpu.regs[RegB::C] & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_232_sbb_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(computer.cpu.regs[RegB::D], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (computer.cpu.regs[RegB::D] & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_233_sbb_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(computer.cpu.regs[RegB::E], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (computer.cpu.regs[RegB::E] & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_234_sbb_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(computer.cpu.regs[RegB::H], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (computer.cpu.regs[RegB::H] & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_235_sbb_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(computer.cpu.regs[RegB::L], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (computer.cpu.regs[RegB::L] & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_236_sbb_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(m, c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (m & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_237_sbb_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(computer.cpu.regs[RegB::A], c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (computer.cpu.regs[RegB::A] & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_240_ana_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] & computer.cpu.regs[RegB::B];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_241_ana_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] & computer.cpu.regs[RegB::C];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_242_ana_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] & computer.cpu.regs[RegB::D];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_243_ana_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] & computer.cpu.regs[RegB::E];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_244_ana_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] & computer.cpu.regs[RegB::H];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_245_ana_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] & computer.cpu.regs[RegB::L];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_246_ana_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let a = computer.cpu.regs[RegB::A] & m;
    computer.cpu.regs[RegB::A] = a;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_247_ana_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A];
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_250_xra_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] ^ computer.cpu.regs[RegB::B];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_251_xra_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] ^ computer.cpu.regs[RegB::C];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_252_xra_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] ^ computer.cpu.regs[RegB::D];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_253_xra_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] ^ computer.cpu.regs[RegB::E];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_254_xra_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] ^ computer.cpu.regs[RegB::H];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_255_xra_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] ^ computer.cpu.regs[RegB::L];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_256_xra_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let a = computer.cpu.regs[RegB::A] ^ m;
    computer.cpu.regs[RegB::A] = a;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_257_xra_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::A] = 0;
    computer.cpu.regs[RegB::F] &= !0x81;
    computer.cpu.regs[RegB::F] |= 0x44;
    4
}

fn e_260_ora_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] | computer.cpu.regs[RegB::B];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_261_ora_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] | computer.cpu.regs[RegB::C];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_262_ora_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] | computer.cpu.regs[RegB::D];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_263_ora_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] | computer.cpu.regs[RegB::E];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_264_ora_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] | computer.cpu.regs[RegB::H];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_265_ora_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let a = computer.cpu.regs[RegB::A] | computer.cpu.regs[RegB::L];
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    4
}

fn e_266_ora_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let a = computer.cpu.regs[RegB::A] | m;
    computer.cpu.regs[RegB::A] = a;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_270_cmp_b(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::B]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::B] << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_271_cmp_c(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::C]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::C] << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_272_cmp_d(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::D]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::D] << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_273_cmp_e(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::E]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::E] << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_274_cmp_h(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::H]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::H] << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_275_cmp_l(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(computer.cpu.regs[RegB::L]);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(computer.cpu.regs[RegB::L] << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    4
}

fn e_276_cmp_m(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let m = computer.mem[computer.cpu.regs[RegW::Hl]];
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(m);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(m << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(m);
    }
    7
}

fn e_277_cmp_a(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegB::F] = 0b01000110;
    4
}

fn e_300_rnz(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x40 == 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_310_rz(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x40 != 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_320_rnc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x01 == 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_330_rc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x01 != 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_340_rpo(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x04 == 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_350_rpe(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x04 != 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_360_rp(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] >> 7 == 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_370_rm(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] >> 7 != 0 {
        e_311_ret(computer, cycles);
        11
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
        5
    }
}

fn e_301_pop_bc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp];
    computer.cpu.regs[RegW::Sp] = sp.wrapping_add(2);
    computer.cpu.regs[RegB::B] = computer.mem[sp.wrapping_add(1)];
    computer.cpu.regs[RegB::C] = computer.mem[sp];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::C]);
        cycles.push(computer.cpu.regs[RegB::B]);
    }
    10
}

fn e_321_pop_de(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp];
    computer.cpu.regs[RegW::Sp] = sp.wrapping_add(2);
    computer.cpu.regs[RegB::D] = computer.mem[sp.wrapping_add(1)];
    computer.cpu.regs[RegB::E] = computer.mem[sp];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::E]);
        cycles.push(computer.cpu.regs[RegB::D]);
    }
    10
}

fn e_341_pop_hl(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp];
    computer.cpu.regs[RegW::Sp] = sp.wrapping_add(2);
    computer.cpu.regs[RegB::H] = computer.mem[sp.wrapping_add(1)];
    computer.cpu.regs[RegB::L] = computer.mem[sp];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::L]);
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    10
}

fn e_361_pop_af(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp];
    computer.cpu.regs[RegW::Sp] = sp.wrapping_add(2);
    computer.cpu.regs[RegB::A] = computer.mem[sp.wrapping_add(1)];
    computer.cpu.regs[RegB::F] = computer.mem[sp] & !0x28 | 0x02;
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::F]);
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    10
}

fn e_311_ret(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let sp = computer.cpu.regs[RegW::Sp];
    computer.cpu.regs[RegW::Sp] = sp.wrapping_add(2);
    computer.cpu.regs[RegB::PcH] = computer.mem[sp.wrapping_add(1)];
    computer.cpu.regs[RegB::PcL] = computer.mem[sp];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::PcL]);
        cycles.push(computer.cpu.regs[RegB::PcH]);
    }
    10
}

fn e_351_pchl(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Hl];
    5
}

fn e_371_sphl(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    computer.cpu.regs[RegW::Sp] = computer.cpu.regs[RegW::Hl];
    5
}

fn e_302_jnz(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x40 == 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_312_jz(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x40 != 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_322_jnc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x01 == 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_332_jc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x01 != 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_342_jpo(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x04 == 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_352_jpe(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x04 != 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_362_jp(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] >> 7 == 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_372_jm(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] >> 7 != 0 {
        e_303_jmp(computer, cycles);
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
    }
    10
}

fn e_303_jmp(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegB::PcH] = computer.mem[pc.wrapping_add(2)];
    computer.cpu.regs[RegB::PcL] = computer.mem[pc.wrapping_add(1)];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::PcL]);
        cycles.push(computer.cpu.regs[RegB::PcH]);
    }
    10
}

fn e_323_out(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    let port = computer.mem[pc.wrapping_add(1)];
    computer.ports[usize::from(port)].out = computer.cpu.regs[RegB::A];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(port);
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    10
}

fn e_333_in(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    let port = computer.mem[pc.wrapping_add(1)];
    computer.cpu.regs[RegB::A] = computer.ports[usize::from(port)].in_;
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    if let Some(cycles) = cycles {
        cycles.push(port);
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    10
}

fn e_343_xthl(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp];
    swap(&mut computer.cpu.regs[RegB::H], &mut computer.mem[sp.wrapping_add(1)]);
    swap(&mut computer.cpu.regs[RegB::L], &mut computer.mem[sp]);
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::L]);
        cycles.push(computer.mem[sp]);
        cycles.push(computer.cpu.regs[RegB::H]);
        cycles.push(computer.mem[sp.wrapping_add(1)]);
    }
    18
}

fn e_353_xchg(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    unsafe { computer.cpu.regs.w.swap(1, 2); }
    4
}

fn e_363_di(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.interrupts_enabled = false;
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    4
}

fn e_373_ei(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.interrupts_enabled = true;
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    4
}

fn e_304_cnz(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x40 == 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_314_cz(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x40 != 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_324_cnc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x01 == 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_334_cc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x01 != 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_344_cpo(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x04 == 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_354_cpe(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] & 0x04 != 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_364_cp(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] >> 7 == 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_374_cm(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    if computer.cpu.regs[RegB::F] >> 7 != 0 {
        e_315_call(computer, cycles);
        17
    } else {
        computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(3);
        11
    }
}

fn e_305_push_bc(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp].wrapping_sub(2);
    computer.cpu.regs[RegW::Sp] = sp;
    computer.mem[sp.wrapping_add(1)] = computer.cpu.regs[RegB::B];
    computer.mem[sp] = computer.cpu.regs[RegB::C];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::C]);
        cycles.push(computer.cpu.regs[RegB::B]);
    }
    11
}

fn e_325_push_de(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp].wrapping_sub(2);
    computer.cpu.regs[RegW::Sp] = sp;
    computer.mem[sp.wrapping_add(1)] = computer.cpu.regs[RegB::D];
    computer.mem[sp] = computer.cpu.regs[RegB::E];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::E]);
        cycles.push(computer.cpu.regs[RegB::D]);
    }
    11
}

fn e_345_push_hl(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp].wrapping_sub(2);
    computer.cpu.regs[RegW::Sp] = sp;
    computer.mem[sp.wrapping_add(1)] = computer.cpu.regs[RegB::H];
    computer.mem[sp] = computer.cpu.regs[RegB::L];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::L]);
        cycles.push(computer.cpu.regs[RegB::H]);
    }
    11
}

fn e_365_push_af(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = computer.cpu.regs[RegW::Pc].wrapping_add(1);
    let sp = computer.cpu.regs[RegW::Sp].wrapping_sub(2);
    computer.cpu.regs[RegW::Sp] = sp;
    computer.mem[sp.wrapping_add(1)] = computer.cpu.regs[RegB::A];
    computer.mem[sp] = computer.cpu.regs[RegB::F];
    if let Some(cycles) = cycles {
        cycles.push(computer.cpu.regs[RegB::F]);
        cycles.push(computer.cpu.regs[RegB::A]);
    }
    11
}

fn e_315_call(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let sp = computer.cpu.regs[RegW::Sp].wrapping_sub(2);
    computer.cpu.regs[RegW::Sp] = sp;
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(3);
    computer.mem[sp.wrapping_add(1)] = computer.cpu.regs[RegB::PcH];
    computer.mem[sp] = computer.cpu.regs[RegB::PcL];
    computer.cpu.regs[RegB::PcH] = computer.mem[pc.wrapping_add(2)];
    computer.cpu.regs[RegB::PcL] = computer.mem[pc.wrapping_add(1)];
    if let Some(cycles) = cycles {
        cycles.push(computer.mem[sp]);
        cycles.push(computer.mem[sp.wrapping_add(1)]);
        cycles.push(computer.cpu.regs[RegB::PcL]);
        cycles.push(computer.cpu.regs[RegB::PcH]);
    }
    17
}

fn e_306_adi(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_add(d);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_add(d << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_316_aci(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].carrying_add(d, c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) + (d & 0x0F) + (c as u8) > 0x0F;
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_326_sui(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(d);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(d << 4);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_336_sbi(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let c = computer.cpu.regs[RegB::F] & 0x01 != 0;
    let (a, cf) = computer.cpu.regs[RegB::A].borrowing_sub(d, c);
    let acf = (computer.cpu.regs[RegB::A] & 0x0F) < (d & 0x0F) + (c as u8);
    computer.cpu.regs[RegB::A] = a;
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_346_ani(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let a = computer.cpu.regs[RegB::A] & d;
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_356_xri(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let a = computer.cpu.regs[RegB::A] ^ d;
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_366_ori(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let a = computer.cpu.regs[RegB::A] | d;
    computer.cpu.regs[RegB::A] = a;
    computer.cpu.regs[RegB::F] &= !0x01;
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_376_cpi(computer: &mut Computer, cycles: Option<&mut MachineCycles>) -> u8 {
    let pc = computer.cpu.regs[RegW::Pc];
    computer.cpu.regs[RegW::Pc] = pc.wrapping_add(2);
    let d = computer.mem[pc.wrapping_add(1)];
    let (a, cf) = computer.cpu.regs[RegB::A].overflowing_sub(d);
    let (_, acf) = (computer.cpu.regs[RegB::A] << 4).overflowing_sub(d << 4);
    Flag::C.set(&mut computer.cpu.regs[RegB::F], cf);
    Flag::Z.set(&mut computer.cpu.regs[RegB::F], a == 0);
    Flag::S.set(&mut computer.cpu.regs[RegB::F], a >> 7 != 0);
    Flag::P.set(&mut computer.cpu.regs[RegB::F], a & 0x01 == 0);
    Flag::Ac.set(&mut computer.cpu.regs[RegB::F], acf);
    if let Some(cycles) = cycles {
        cycles.push(d);
    }
    7
}

fn e_307_rst_0(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o000;
    11
}

fn e_317_rst_1(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o010;
    11
}

fn e_327_rst_2(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o020;
    11
}

fn e_337_rst_3(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o030;
    11
}

fn e_347_rst_4(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o040;
    11
}

fn e_357_rst_5(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o050;
    11
}

fn e_367_rst_6(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o060;
    11
}

fn e_377_rst_7(computer: &mut Computer, _cycles: Option<&mut MachineCycles>) -> u8 {
    computer.cpu.regs[RegW::Pc] = 0o070;
    11
}
