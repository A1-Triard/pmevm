use core::fmt::{self, Display, Formatter};
use crate::base::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct ProgramLine {
    pub addr_hi: u8,
    pub addr_low: u8,
    pub code: u8,
    pub mnemonic: &'static str,
}

impl Display for ProgramLine {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:03o} {:03o} {:03o}", self.addr_hi, self.addr_low, self.code)?;
        if !self.mnemonic.is_empty() {
            write!(f, " {}", self.mnemonic)?;
        }
        Ok(())
    }
}

pub trait ComputerProgramExt {
    fn poke_program<'a>(&mut self, lines: impl IntoIterator<Item=&'a ProgramLine>);
}

impl ComputerProgramExt for Computer {
    fn poke_program<'a>(&mut self, lines: impl IntoIterator<Item=&'a ProgramLine>) {
        for line in lines.into_iter() {
            self.poke(((line.addr_hi as u16) << 8) | (line.addr_low as u16), line.code);
        }
    }
}

#[doc(hidden)]
pub const fn octal(d: u16) -> u8 {
    let hundreds = d / 100;
    let tens = (d / 10) % 10;
    let ones = d % 10;
    assert!(hundreds < 4 && tens < 8 && ones < 8, "invalid octal byte literal");
    ((hundreds as u8) << 6) | ((tens as u8) << 3) | (ones as u8)
}

pub struct Program(pub &'static [ProgramLine]);

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for line in self.0 {
            writeln!(f, "{}", line)?;
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! program {
    (
        $($(
            $addr_hi:literal
            $addr_low:literal
            $code:literal
            $mnemonic:literal
        ),+ $(,)?)?
    ) => {
        $crate::Program(&[
            $($(
                $crate::ProgramLine {
                    addr_hi: $crate::octal($addr_hi),
                    addr_low: $crate::octal($addr_low),
                    code: $crate::octal($code),
                    mnemonic: $mnemonic
                }
            ),+)?
        ])
    };
}
