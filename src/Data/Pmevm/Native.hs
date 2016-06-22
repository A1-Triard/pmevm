module Data.Pmevm.Native where

#include <haskell>

data PSW = PSW
  { fZero :: !Bool
  , fCarry :: !Bool
  , fSign :: !Bool
  , fParity :: !Bool
  , fAuxCarry :: !Bool
  } deriving (Eq, Show, Ord)

flagValue :: Bool -> Int
flagValue False = 0
flagValue True = 1

pswValue :: PSW -> Int
pswValue p
  = (flagValue $ fCarry p)
  + 1 * 2
  + (flagValue $ fParity p) * 4
  + (flagValue $ fAuxCarry p) * 16
  + (flagValue $ fZero p) * 64
  + (flagValue $ fSign p) * 128

data CPURegister = R_A | R_B | R_C | R_D | R_E | R_H | R_L | R_M deriving (Eq, Show, Ord, Enum)

rOpCode :: CPURegister -> Int
rOpCode R_A = 7
rOpCode R_B = 0
rOpCode R_C = 1
rOpCode R_D = 2
rOpCode R_E = 3
rOpCode R_H = 4
rOpCode R_L = 5
rOpCode R_M = 6

data CPURegisterExt = E_BC | E_DE deriving (Eq, Show, Ord, Enum)
data CPURegisterPair = R_BC | R_DE | R_HL | R_SP deriving (Eq, Show, Ord, Enum)
data CPURegisterWord = W_BC | W_DE | W_HL | W_APSW deriving (Eq, Show, Ord, Enum)

eOpCode :: CPURegisterExt -> Int
eOpCode E_BC = 0
eOpCode E_DE = 2

pOpCode :: CPURegisterPair -> Int
pOpCode R_BC = 0
pOpCode R_DE = 2
pOpCode R_HL = 4
pOpCode R_SP = 6

wOpCode :: CPURegisterWord -> Int
wOpCode W_BC = 0
wOpCode W_DE = 2
wOpCode W_HL = 4
wOpCode W_APSW = 6

data CPUFlag = C_NZ | C_Z | C_NC | C_C | C_PO | C_PE | C_P | C_M deriving (Eq, Show, Ord, Enum)

fOpCode :: CPUFlag -> Int
fOpCode C_NZ = 0
fOpCode C_Z = 1
fOpCode C_NC = 2
fOpCode C_C = 3
fOpCode C_PO = 4
fOpCode C_PE = 5
fOpCode C_P = 6
fOpCode C_M = 7

checkFlag :: CPUFlag -> PSW -> Bool
checkFlag C_NZ = not . fZero
checkFlag C_Z = fZero
checkFlag C_NC = not . fCarry
checkFlag C_C = fCarry
checkFlag C_PO = not . fParity
checkFlag C_PE = fParity
checkFlag C_P = not . fSign
checkFlag C_M = fSign

data CPUCommand
  = INR !CPURegister
  | DCR !CPURegister
  | MOV !CPURegister !CPURegister
  | ADD !CPURegister
  | ADC !CPURegister
  | SUB !CPURegister
  | SBB !CPURegister
  | ANA !CPURegister
  | XRA !CPURegister
  | ORA !CPURegister
  | CMP !CPURegister
  | INX !CPURegisterPair
  | DCX !CPURegisterPair
  | DAD !CPURegisterPair
  | POP !CPURegisterWord
  | PUSH !CPURegisterWord
  | STAX !CPURegisterExt
  | LDAX !CPURegisterExt
  | RCC !CPUFlag
  | RET
  | RLC | RRC | RAL | RAR
  | XCHG | XTHL | SPHL | PCHL
  | HLT
  | NOP
  | DI | EI
  | DAA | CMA
  | STC | CMC
  | RST Int
  | ADI | ACI | SUI | SBI | ANI | XRI | ORI | CPI
  | IN | OUT
  | MVI !CPURegister
  | JCC !CPUFlag
  | JMP
  | CCC !CPUFlag
  | CALL
  | LXI !CPURegisterPair
  | STA | LDA | SHLD | LHLD

octets :: Int -> Int -> Int -> Int
octets a b c = a * 64 + b * 8 + c

cOpCode :: CPUCommand -> Int
cOpCode (INR r) = octets 0 (rOpCode r) 4
cOpCode (DCR r) = octets 0 (rOpCode r) 5
cOpCode (MOV rd rs) = octets 1 (rOpCode rd) (rOpCode rs)
cOpCode (ADD r) = octets 2 0 (rOpCode r)
cOpCode (ADC r) = octets 2 1 (rOpCode r)
cOpCode (SUB r) = octets 2 2 (rOpCode r)
cOpCode (SBB r) = octets 2 3 (rOpCode r)
cOpCode (ANA r) = octets 2 4 (rOpCode r)
cOpCode (XRA r) = octets 2 5 (rOpCode r)
cOpCode (ORA r) = octets 2 6 (rOpCode r)
cOpCode (CMP r) = octets 2 7 (rOpCode r)
cOpCode (INX rp) = octets 0 (pOpCode rp) 3
cOpCode (DCX rp) = octets 0 (1 + pOpCode rp) 3
cOpCode (DAD rp) = octets 0 (1 + pOpCode rp) 1
cOpCode (POP rp) = octets 3 (wOpCode rp) 1
cOpCode (PUSH rp) = octets 3 (wOpCode rp) 5
cOpCode (STAX rp) = octets 0 (eOpCode rp) 2
cOpCode (LDAX rp) = octets 0 (1 + eOpCode rp) 2
cOpCode (RCC f) = octets 3 (fOpCode f) 0
cOpCode RET = 0o311
cOpCode RLC = 0o007
cOpCode RRC = 0o017
cOpCode RAL = 0o027
cOpCode RAR = 0o037
cOpCode XCHG = 0o353
cOpCode XTHL = 0o343
cOpCode SPHL = 0o371
cOpCode PCHL = 0o351
cOpCode HLT = 0o166
cOpCode NOP = 0o000
cOpCode DI = 0o363
cOpCode EI = 0o373
cOpCode DAA = 0o047
cOpCode CMA = 0o057
cOpCode STC = 0o067
cOpCode CMC = 0o077
cOpCode (RST a) = octets 3 a 7
cOpCode ADI = 0o306
cOpCode ACI = 0o316
cOpCode SUI = 0o326
cOpCode SBI = 0o336
cOpCode ANI = 0o346
cOpCode XRI = 0o356
cOpCode ORI = 0o366
cOpCode CPI = 0o376
cOpCode IN = 0o333
cOpCode OUT = 0o323
cOpCode (MVI r) = octets 0 (rOpCode r) 6
cOpCode (JCC f) = octets 3 (fOpCode f) 2
cOpCode JMP = 0o303
cOpCode (CCC f) = octets 3 (fOpCode f) 4
cOpCode CALL = 0o315
cOpCode (LXI rp) = octets 0 (pOpCode rp) 1
cOpCode STA = 0o062
cOpCode LDA = 0o072
cOpCode SHLD = 0o042
cOpCode LHLD = 0o052

cycles :: CPUCommand -> PSW -> Int
cycles (INR R_M) _ = 10
cycles (INR _) _ = 5
cycles (DCR R_M) _ = 10
cycles (DCR _) _ = 5
cycles (MOV R_M R_M) _ = 0
cycles (MOV R_M _) _ = 7
cycles (MOV _ R_M) _ = 7
cycles (MOV _ _) _ = 5
cycles (ADD R_M) _ = 7
cycles (ADD _) _ = 4
cycles (ADC R_M) _ = 7
cycles (ADC _) _ = 4
cycles (SUB R_M) _ = 7
cycles (SUB _) _ = 4
cycles (SBB R_M) _ = 7
cycles (SBB _) _ = 4
cycles (ANA R_M) _ = 7
cycles (ANA _) _ = 4
cycles (XRA R_M) _ = 7
cycles (XRA _) _ = 4
cycles (ORA R_M) _ = 7
cycles (ORA _) _ = 4
cycles (CMP R_M) _ = 7
cycles (CMP _) _ = 4
cycles (INX _) _ = 5
cycles (DCX _) _ = 5
cycles (DAD _) _ = 10
cycles (POP _) _ = 10
cycles (PUSH _) _ = 11
cycles (STAX _) _ = 7
cycles (LDAX _) _ = 7
cycles (RCC f) p = if checkFlag f p then 11 else 5
cycles RET _ = 10
cycles RLC _ = 4
cycles RRC _ = 4
cycles RAL _ = 4
cycles RAR _ = 4
cycles XCHG _ = 4
cycles XTHL _ = 18
cycles SPHL _ = 5
cycles PCHL _ = 5
cycles HLT _ = 7
cycles NOP _ = 4
cycles DI _ = 4
cycles EI _ = 4
cycles DAA _ = 4
cycles CMA _ = 4
cycles STC _ = 4
cycles CMC _ = 4
cycles (RST _) _ = 11
cycles ADI _ = 7
cycles ACI _ = 7
cycles SUI _ = 7
cycles SBI _ = 7
cycles ANI _ = 7
cycles XRI _ = 7
cycles ORI _ = 7
cycles CPI _ = 7
cycles IN _ = 10
cycles OUT _ = 10
cycles (MVI R_M) _ = 10
cycles (MVI _) _ = 7
cycles (JCC _) _ = 10
cycles JMP _ = 10
cycles (CCC f) p = if checkFlag f p then 17 else 11
cycles CALL _ = 17
cycles (LXI _) _ = 10
cycles STA _ = 13
cycles LDA _ = 13
cycles SHLD _ = 16
cycles LHLD _ = 16
