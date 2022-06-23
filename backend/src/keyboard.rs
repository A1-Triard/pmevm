use enumn::N;
use crate::base::*;

#[derive(N, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
#[repr(u8)]
pub enum Key {
    K0 = 0x0, K1 = 0x1, K2 = 0x2, K3 = 0x3,
    K4 = 0x4, K5 = 0x5, K6 = 0x6, K7 = 0x7,
    K8 = 0x8, K9 = 0x9, KA = 0xA, KB = 0xB,
    KC = 0xC, KD = 0xD, KE = 0xE, KF = 0xF,
}

#[derive(Debug, Clone, Copy)]
pub struct Keyboard(u16);

impl Keyboard {
    pub fn new() -> Keyboard {
        Keyboard(0)
    }

    pub fn get(self, key: Key) -> bool {
        self.0 & (1 << (key as u8)) != 0
    }

    pub fn set(&mut self, key: Key, pressed: bool) {
        let mask = 1 << (key as u8);
        let f = (pressed as u16).wrapping_neg();
        self.0 = ((self.0 ^ f) & !mask) ^ f;
    }

    pub fn step(self, computer: &mut Computer) {
        let scan = computer.peek_port(3);
        let c_0 = scan & 0x08 != 0;
        let c_1 = scan & 0x04 != 0;
        let c_2 = scan & 0x02 != 0;
        let c_3 = scan & 0x01 != 0;
        let r_3 =
            (c_0 || !self.get(Key::K0)) &&
            (c_1 || !self.get(Key::K1)) &&
            (c_2 || !self.get(Key::K2)) &&
            (c_3 || !self.get(Key::K3));
        let r_2 =
            (c_0 || !self.get(Key::K4)) &&
            (c_1 || !self.get(Key::K5)) &&
            (c_2 || !self.get(Key::K6)) &&
            (c_3 || !self.get(Key::K7));
        let r_1 =
            (c_0 || !self.get(Key::K8)) &&
            (c_1 || !self.get(Key::K9)) &&
            (c_2 || !self.get(Key::KA)) &&
            (c_3 || !self.get(Key::KB));
        let r_0 =
            (c_0 || !self.get(Key::KC)) &&
            (c_1 || !self.get(Key::KD)) &&
            (c_2 || !self.get(Key::KE)) &&
            (c_3 || !self.get(Key::KF));
        let answer = ((r_0 as u8) << 3)  | ((r_1 as u8) << 2) | ((r_2 as u8) << 1) | (r_3 as u8);
        computer.poke_port(3, answer);
    }
}
