module Data.Pmevm.Native where

#include <haskell>

data PSW = PSW
  { fZero :: !Bool
  , fCarry :: !Bool
  , fSign :: !Bool
  , fParity :: !Bool
  , fAuxCarry :: !Bool
  } deriving (Eq, Show, Ord)

flagCode :: Bool -> Word8
flagCode False = 0
flagCode True = 1

cpuFlag :: Word8 -> Bool
cpuFlag 0 = False
cpuFlag _ = True

pswCode :: PSW -> Word8
pswCode p
  = (flagCode $ fCarry p)
  + 1 * 2
  + (flagCode $ fParity p) * 4
  + (flagCode $ fAuxCarry p) * 16
  + (flagCode $ fZero p) * 64
  + (flagCode $ fSign p) * 128

pswScan :: Word8 -> PSW
pswScan w = PSW
  (cpuFlag $ w .&. 0x80 `shift` (-6))
  (cpuFlag $ w .&. 0x01)
  (cpuFlag $ w .&. 0xF0 `shift` (-7))
  (cpuFlag $ w .&. 0x04 `shift` (-2))
  (cpuFlag $ w .&. 0x10 `shift` (-4))

pswUpdate :: PSW -> Word8 -> PSW
pswUpdate p w = p
  { fZero = w == 0
  , fSign = w .&. 0xF0 /= 0
  , fParity = w .&. 0x01 == 0
  }

data CPURegister = R_A | R_B | R_C | R_D | R_E | R_H | R_L | R_M deriving (Eq, Show, Ord, Enum)

registerCode :: CPURegister -> Word8
registerCode R_A = 7
registerCode R_B = 0
registerCode R_C = 1
registerCode R_D = 2
registerCode R_E = 3
registerCode R_H = 4
registerCode R_L = 5
registerCode R_M = 6

cpuRegister :: Word8 -> CPURegister
cpuRegister 0 = R_B
cpuRegister 1 = R_C
cpuRegister 2 = R_D
cpuRegister 3 = R_E
cpuRegister 4 = R_H
cpuRegister 5 = R_L
cpuRegister 6 = R_M
cpuRegister 7 = R_A
cpuRegister _ = error "cpuRegister"

data CPURegisterExt = E_BC | E_DE deriving (Eq, Show, Ord, Enum)
data CPURegisterPair = R_BC | R_DE | R_HL | R_SP deriving (Eq, Show, Ord, Enum)
data CPURegisterWord = W_BC | W_DE | W_HL | W_PSWA deriving (Eq, Show, Ord, Enum)

extRegisterCode :: CPURegisterExt -> Word8
extRegisterCode E_BC = 0
extRegisterCode E_DE = 2

cpuRegisterExt :: Word8 -> CPURegisterExt
cpuRegisterExt 0 = E_BC
cpuRegisterExt 2 = E_DE
cpuRegisterExt _ = error "cpuRegisterExt"

registerPairCode :: CPURegisterPair -> Word8
registerPairCode R_BC = 0
registerPairCode R_DE = 2
registerPairCode R_HL = 4
registerPairCode R_SP = 6

cpuRegisterPair :: Word8 -> CPURegisterPair
cpuRegisterPair 0 = R_BC
cpuRegisterPair 2 = R_DE
cpuRegisterPair 4 = R_HL
cpuRegisterPair 6 = R_SP
cpuRegisterPair x = error $ "cpuRegisterPair " ++ show x

registerWordCode :: CPURegisterWord -> Word8
registerWordCode W_BC = 0
registerWordCode W_DE = 2
registerWordCode W_HL = 4
registerWordCode W_PSWA = 6

cpuRegisterWord :: Word8 -> CPURegisterWord
cpuRegisterWord 0 = W_BC
cpuRegisterWord 2 = W_DE
cpuRegisterWord 4 = W_HL
cpuRegisterWord 6 = W_PSWA
cpuRegisterWord _ = error "cpuRegisterWord"

data CPUCondition = C_NZ | C_Z | C_NC | C_C | C_PO | C_PE | C_P | C_M deriving (Eq, Show, Ord, Enum)

conditionCode :: CPUCondition -> Word8
conditionCode C_NZ = 0
conditionCode C_Z = 1
conditionCode C_NC = 2
conditionCode C_C = 3
conditionCode C_PO = 4
conditionCode C_PE = 5
conditionCode C_P = 6
conditionCode C_M = 7

cpuCondition :: Word8 -> CPUCondition
cpuCondition 0 = C_NZ
cpuCondition 1 = C_Z
cpuCondition 2 = C_NC
cpuCondition 3 = C_C
cpuCondition 4 = C_PO
cpuCondition 5 = C_PE
cpuCondition 6 = C_P
cpuCondition 7 = C_M
cpuCondition _ = error "cpuCondition"

fitCondition :: CPUCondition -> PSW -> Bool
fitCondition C_NZ = not . fZero
fitCondition C_Z = fZero
fitCondition C_NC = not . fCarry
fitCondition C_C = fCarry
fitCondition C_PO = not . fParity
fitCondition C_PE = fParity
fitCondition C_P = not . fSign
fitCondition C_M = fSign

data CPUOperation
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
  | RCC !CPUCondition
  | RET | RET'
  | RLC | RRC | RAL | RAR
  | XCHG | XTHL | SPHL | PCHL
  | NOP !Word8
  | DI | EI
  | DAA | CMA
  | STC | CMC
  | RST !Word8
  | ADI | ACI | SUI | SBI | ANI | XRI | ORI | CPI
  | IN | OUT
  | MVI !CPURegister
  | JCC !CPUCondition
  | JMP | JMP'
  | CCC !CPUCondition
  | CALL !CPURegisterPair
  | LXI !CPURegisterPair
  | STA | LDA | SHLD | LHLD
  deriving (Eq, Ord)

operationTicks :: CPUOperation -> PSW -> Int
operationTicks (INR R_M) _ = 10
operationTicks (INR _) _ = 5
operationTicks (DCR R_M) _ = 10
operationTicks (DCR _) _ = 5
operationTicks (MOV R_M R_M) _ = 7
operationTicks (MOV R_M _) _ = 7
operationTicks (MOV _ R_M) _ = 7
operationTicks (MOV _ _) _ = 5
operationTicks (ADD R_M) _ = 7
operationTicks (ADD _) _ = 4
operationTicks (ADC R_M) _ = 7
operationTicks (ADC _) _ = 4
operationTicks (SUB R_M) _ = 7
operationTicks (SUB _) _ = 4
operationTicks (SBB R_M) _ = 7
operationTicks (SBB _) _ = 4
operationTicks (ANA R_M) _ = 7
operationTicks (ANA _) _ = 4
operationTicks (XRA R_M) _ = 7
operationTicks (XRA _) _ = 4
operationTicks (ORA R_M) _ = 7
operationTicks (ORA _) _ = 4
operationTicks (CMP R_M) _ = 7
operationTicks (CMP _) _ = 4
operationTicks (INX _) _ = 5
operationTicks (DCX _) _ = 5
operationTicks (DAD _) _ = 10
operationTicks (POP _) _ = 10
operationTicks (PUSH _) _ = 11
operationTicks (STAX _) _ = 7
operationTicks (LDAX _) _ = 7
operationTicks (RCC f) p = if fitCondition f p then 11 else 5
operationTicks RET _ = 10
operationTicks RET' _ = 10
operationTicks RLC _ = 4
operationTicks RRC _ = 4
operationTicks RAL _ = 4
operationTicks RAR _ = 4
operationTicks XCHG _ = 4
operationTicks XTHL _ = 18
operationTicks SPHL _ = 5
operationTicks PCHL _ = 5
operationTicks (NOP _) _ = 4
operationTicks DI _ = 4
operationTicks EI _ = 4
operationTicks DAA _ = 4
operationTicks CMA _ = 4
operationTicks STC _ = 4
operationTicks CMC _ = 4
operationTicks (RST _) _ = 11
operationTicks ADI _ = 7
operationTicks ACI _ = 7
operationTicks SUI _ = 7
operationTicks SBI _ = 7
operationTicks ANI _ = 7
operationTicks XRI _ = 7
operationTicks ORI _ = 7
operationTicks CPI _ = 7
operationTicks IN _ = 10
operationTicks OUT _ = 10
operationTicks (MVI R_M) _ = 10
operationTicks (MVI _) _ = 7
operationTicks (JCC _) _ = 10
operationTicks JMP _ = 10
operationTicks JMP' _ = 10
operationTicks (CCC f) p = if fitCondition f p then 17 else 11
operationTicks (CALL _) _ = 17
operationTicks (LXI _) _ = 10
operationTicks STA _ = 13
operationTicks LDA _ = 13
operationTicks SHLD _ = 16
operationTicks LHLD _ = 16

fromOctets :: Word8 -> Word8 -> Word8 -> Word8
fromOctets a b c = a * 64 + b * 8 + c

operationCode :: CPUOperation -> Word8
operationCode SHLD = 0o042
operationCode LHLD = 0o052
operationCode STA = 0o062
operationCode LDA = 0o072
operationCode RLC = 0o007
operationCode RRC = 0o017
operationCode RAL = 0o027
operationCode RAR = 0o037
operationCode DAA = 0o047
operationCode CMA = 0o057
operationCode STC = 0o067
operationCode CMC = 0o077
operationCode RET = 0o311
operationCode RET' = 0o331
operationCode PCHL = 0o351
operationCode SPHL = 0o371
operationCode JMP = 0o303
operationCode JMP' = 0o313
operationCode OUT = 0o323
operationCode IN = 0o333
operationCode XTHL = 0o343
operationCode XCHG = 0o353
operationCode DI = 0o363
operationCode EI = 0o373
operationCode ADI = 0o306
operationCode ACI = 0o316
operationCode SUI = 0o326
operationCode SBI = 0o336
operationCode ANI = 0o346
operationCode XRI = 0o356
operationCode ORI = 0o366
operationCode CPI = 0o376
operationCode (MOV rd rs) = fromOctets 1 (registerCode rd) (registerCode rs)
operationCode (ADD r) = fromOctets 2 0 (registerCode r)
operationCode (ADC r) = fromOctets 2 1 (registerCode r)
operationCode (SUB r) = fromOctets 2 2 (registerCode r)
operationCode (SBB r) = fromOctets 2 3 (registerCode r)
operationCode (ANA r) = fromOctets 2 4 (registerCode r)
operationCode (XRA r) = fromOctets 2 5 (registerCode r)
operationCode (ORA r) = fromOctets 2 6 (registerCode r)
operationCode (CMP r) = fromOctets 2 7 (registerCode r)
operationCode (NOP a) = fromOctets 0 a 0
operationCode (LXI rp) = fromOctets 0 (registerPairCode rp) 1
operationCode (DAD rp) = fromOctets 0 (registerPairCode rp + 1) 1
operationCode (STAX rp) = fromOctets 0 (extRegisterCode rp) 2
operationCode (LDAX rp) = fromOctets 0 (extRegisterCode rp + 1) 2
operationCode (INX rp) = fromOctets 0 (registerPairCode rp) 3
operationCode (DCX rp) = fromOctets 0 (registerPairCode rp + 1) 3
operationCode (INR r) = fromOctets 0 (registerCode r) 4
operationCode (DCR r) = fromOctets 0 (registerCode r) 5
operationCode (MVI r) = fromOctets 0 (registerCode r) 6
operationCode (RCC f) = fromOctets 3 (conditionCode f) 0
operationCode (POP rp) = fromOctets 3 (registerWordCode rp) 1
operationCode (JCC f) = fromOctets 3 (conditionCode f) 2
operationCode (CCC f) = fromOctets 3 (conditionCode f) 4
operationCode (PUSH rp) = fromOctets 3 (registerWordCode rp) 5
operationCode (CALL rp) = fromOctets 3 (registerPairCode rp + 1) 5
operationCode (RST a) = fromOctets 3 a 7

octet3 :: Word8 -> Word8
octet3 w = w .&. 0o007

octet2 :: Word8 -> Word8
octet2 w = (w .&. 0o070) `shift` (-3)

cpuOperation :: Word8 -> CPUOperation
cpuOperation 0o042 = SHLD
cpuOperation 0o052 = LHLD
cpuOperation 0o062 = STA
cpuOperation 0o072 = LDA
cpuOperation 0o007 = RLC
cpuOperation 0o017 = RRC
cpuOperation 0o027 = RAL
cpuOperation 0o037 = RAR
cpuOperation 0o047 = DAA
cpuOperation 0o057 = CMA
cpuOperation 0o067 = STC
cpuOperation 0o077 = CMC
cpuOperation 0o311 = RET
cpuOperation 0o331 = RET'
cpuOperation 0o351 = PCHL
cpuOperation 0o371 = SPHL
cpuOperation 0o303 = JMP
cpuOperation 0o313 = JMP'
cpuOperation 0o323 = OUT
cpuOperation 0o333 = IN
cpuOperation 0o343 = XTHL
cpuOperation 0o353 = XCHG
cpuOperation 0o363 = DI
cpuOperation 0o373 = EI
cpuOperation 0o306 = ADI
cpuOperation 0o316 = ACI
cpuOperation 0o326 = SUI
cpuOperation 0o336 = SBI
cpuOperation 0o346 = ANI
cpuOperation 0o356 = XRI
cpuOperation 0o366 = ORI
cpuOperation 0o376 = CPI
cpuOperation op_code
  | op_code .&. 0o300 == 0o100 = MOV (cpuRegister $ octet2 op_code) (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o200 = ADD (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o210 = ADC (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o220 = SUB (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o230 = SBB (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o240 = ANA (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o250 = XRA (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o260 = ORA (cpuRegister $ octet3 op_code)
  | op_code .&. 0o370 == 0o270 = CMP (cpuRegister $ octet3 op_code)
  | op_code .&. 0o307 == 0o000 = NOP $ octet2 op_code
  | op_code .&. 0o317 == 0o001 = LXI (cpuRegisterPair $ octet2 op_code)
  | op_code .&. 0o317 == 0o011 = DAD (cpuRegisterPair $ octet2 op_code - 1)
  | op_code .&. 0o317 == 0o002 = STAX (cpuRegisterExt $ octet2 op_code)
  | op_code .&. 0o317 == 0o012 = LDAX (cpuRegisterExt $ octet2 op_code - 1)
  | op_code .&. 0o317 == 0o003 = INX (cpuRegisterPair $ octet2 op_code)
  | op_code .&. 0o317 == 0o013 = DCX (cpuRegisterPair $ octet2 op_code - 1)
  | op_code .&. 0o307 == 0o004 = INR (cpuRegister $ octet2 op_code)
  | op_code .&. 0o307 == 0o005 = DCR (cpuRegister $ octet2 op_code)
  | op_code .&. 0o307 == 0o006 = MVI (cpuRegister $ octet2 op_code)
  | op_code .&. 0o307 == 0o300 = RCC (cpuCondition $ octet2 op_code)
  | op_code .&. 0o317 == 0o301 = POP (cpuRegisterWord $ octet2 op_code)
  | op_code .&. 0o307 == 0o302 = JCC (cpuCondition $ octet2 op_code)
  | op_code .&. 0o307 == 0o304 = CCC (cpuCondition $ octet2 op_code)
  | op_code .&. 0o317 == 0o305 = PUSH (cpuRegisterWord $ octet2 op_code)
  | op_code .&. 0o317 == 0o315 = CALL (cpuRegisterPair $ octet2 op_code - 1)
  | op_code .&. 0o307 == 0o307 = RST $ octet2 op_code
  | otherwise = error $ "cpuOperation " ++ show op_code

type Memory = Vector Word8

initMemory :: Memory
initMemory = V.replicate (fromIntegral (maxBound :: Word16) + 1) 0

type Ports = Vector Word8

initPorts :: Ports
initPorts = V.replicate (fromIntegral (maxBound :: Word8) + 1) 0

data CPU = CPU
  { isHalted :: !Bool
  , interruptsEnabled :: !Bool
  , timer :: Int
  , psw :: !PSW
  , regA :: !Word8
  , regB :: !Word8
  , regC :: !Word8
  , regD :: !Word8
  , regE :: !Word8
  , regH :: !Word8
  , regL :: !Word8
  , regSP :: !Word16
  , regPC :: !Word16
  }

getReg :: CPURegister -> Memory -> CPU -> Word8
getReg R_A = const regA
getReg R_B = const regB
getReg R_C = const regC
getReg R_D = const regD
getReg R_E = const regE
getReg R_H = const regH
getReg R_L = const regL
getReg R_M = readMemory

setReg :: CPURegister -> Word8 -> Computer -> Computer
setReg R_A w (Computer o m p) = Computer o m $ p { regA = w }
setReg R_B w (Computer o m p) = Computer o m $ p { regB = w }
setReg R_C w (Computer o m p) = Computer o m $ p { regC = w }
setReg R_D w (Computer o m p) = Computer o m $ p { regD = w }
setReg R_E w (Computer o m p) = Computer o m $ p { regE = w }
setReg R_H w (Computer o m p) = Computer o m $ p { regH = w }
setReg R_L w (Computer o m p) = Computer o m $ p { regL = w }
setReg R_M w (Computer o m p) = Computer o (writeMemory m p w) p

getRegPair :: CPURegisterPair -> CPU -> Word16
getRegPair R_BC p = fromIntegral (regB p) * 256 + fromIntegral (regC p)
getRegPair R_DE p = fromIntegral (regD p) * 256 + fromIntegral (regE p)
getRegPair R_HL p = fromIntegral (regH p) * 256 + fromIntegral (regL p)
getRegPair R_SP p = regSP p

setRegPair :: CPURegisterPair -> Word16 -> CPU -> CPU
setRegPair R_BC w p = p { regB = fromIntegral ((w .&. 0xFF00) `shift` (-8)), regC = fromIntegral (w .&. 0x00FF) }
setRegPair R_DE w p = p { regD = fromIntegral ((w .&. 0xFF00) `shift` (-8)), regE = fromIntegral (w .&. 0x00FF) }
setRegPair R_HL w p = p { regH = fromIntegral ((w .&. 0xFF00) `shift` (-8)), regL = fromIntegral (w .&. 0x00FF) }
setRegPair R_SP w p = p { regSP = w }

getRegWord :: CPURegisterWord -> CPU -> Word16
getRegWord W_BC p = fromIntegral (regB p) * 256 + fromIntegral (regC p)
getRegWord W_DE p = fromIntegral (regD p) * 256 + fromIntegral (regE p)
getRegWord W_HL p = fromIntegral (regH p) * 256 + fromIntegral (regL p)
getRegWord W_PSWA p = fromIntegral (pswCode $ psw p) * 256 + fromIntegral (regA p)

setRegWord :: CPURegisterWord -> Word16 -> CPU -> CPU
setRegWord W_BC w p = p { regB = fromIntegral ((w .&. 0xFF00) `shift` (-8)), regC = fromIntegral (w .&. 0x00FF) }
setRegWord W_DE w p = p { regD = fromIntegral ((w .&. 0xFF00) `shift` (-8)), regE = fromIntegral (w .&. 0x00FF) }
setRegWord W_HL w p = p { regH = fromIntegral ((w .&. 0xFF00) `shift` (-8)), regL = fromIntegral (w .&. 0x00FF) }
setRegWord W_PSWA w p = p { psw = pswScan $ fromIntegral ((w .&. 0xFF00) `shift` (-8)), regA = fromIntegral (w .&. 0x00FF) }

getRegExt :: CPURegisterExt -> CPU -> Word16
getRegExt E_BC p = fromIntegral (regB p) * 256 + fromIntegral (regC p)
getRegExt E_DE p = fromIntegral (regD p) * 256 + fromIntegral (regE p)

readMemory :: Memory -> CPU -> Word8
readMemory m p =
  let addr = fromIntegral (regH p) * 256 + fromIntegral (regL p) in
  fromMaybe 0 $ m !? (fromIntegral (addr :: Word16))

writeMemory :: Memory -> CPU -> Word8 -> Memory
writeMemory m p w =
  let addr = fromIntegral (regH p) * 256 + fromIntegral (regL p) in
  m // [(fromIntegral (addr :: Word16), w)]

asHalted :: CPU -> Maybe CPU
asHalted p = if isHalted p then Just p else Nothing

initCPU :: CPU
initCPU = CPU False True 0 (pswScan 0) 0 0 0 0 0 0 0 0 0

data Computer = Computer { ports :: Ports, memory :: Memory, cpu :: CPU }

initComputer :: Computer
initComputer = Computer initPorts initMemory initCPU

loadByte :: Memory -> CPU -> Word8
loadByte m p = fromMaybe 0 $ m !? (fromIntegral $ regPC p + 1)

loadWord :: Memory -> CPU -> Word16
loadWord m p =
  let l = fromMaybe 0 $ m !? (fromIntegral $ regPC p + 1) in
  let h = fromMaybe 0 $ m !? (fromIntegral $ regPC p + 2) in
  fromIntegral h * 256 + fromIntegral l

executeReturn :: Computer -> Computer
executeReturn (Computer o m p) =
  let l = fromMaybe 0 $ m !? (fromIntegral $ regSP p) in
  let h = fromMaybe 0 $ m !? (fromIntegral $ regSP p + 1) in
  Computer o m $ p { regPC = fromIntegral h * 256 + fromIntegral l, regSP = regSP p + 2 }

executeJump :: Computer -> Computer
executeJump (Computer o m p) =
  let addr = loadWord m p in
  Computer o m $ p { regPC = addr }

executeCall :: Computer -> Computer
executeCall (Computer o m p) =
  let addr = loadWord m p in
  let ret_addr = regPC p + 3 in
  let ret_h = fromIntegral $ (ret_addr .&. 0xFF00) `shift` (-8) in
  let ret_l = fromIntegral $ ret_addr .&. 0x00FF in
  let m' = m // [(fromIntegral (regSP p - 1), ret_h), (fromIntegral (regSP p - 2), ret_l)] in
  Computer o m' $ p { regSP = regSP p - 2, regPC = addr }

executeOperation :: CPUOperation -> Computer -> Computer
executeOperation _ (Computer o m (asHalted -> Just p)) = Computer o m p
executeOperation SHLD (Computer o m p) =
  let addr = loadWord m p in
  let m' = m // [(fromIntegral addr, regL p), (fromIntegral (addr + 1), regH p)] in
  Computer o m' $ p { regPC = regPC p + 3 }
executeOperation LHLD (Computer o m p) =
  let addr = loadWord m p in
  let l = fromMaybe 0 $ m !? (fromIntegral addr) in
  let h = fromMaybe 0 $ m !? (fromIntegral $ addr + 1) in
  Computer o m $ p { regH = h, regL = l, regPC = regPC p + 3 }
executeOperation STA (Computer o m p) =
  let addr = loadWord m p in
  let m' = m // [(fromIntegral addr, regA p)] in
  Computer o m' $ p { regPC = regPC p + 3 }
executeOperation LDA (Computer o m p) =
  let addr = loadWord m p in
  let a = fromMaybe 0 $ m !? (fromIntegral addr) in
  Computer o m $ p { regA = a, regPC = regPC p + 3 }
executeOperation RLC (Computer o m p) =
  let c = cpuFlag $ regA p .&. 0x80 `shift` (-7) in
  let a = regA p `rotate` 1 in
  Computer o m $ p { regA = a, psw = (psw p) { fCarry = c }, regPC = regPC p + 1 }
executeOperation RRC (Computer o m p) =
  let c = cpuFlag $ regA p .&. 0x01 in
  let a = regA p `rotate` (-1) in
  Computer o m $ p { regA = a, psw = (psw p) { fCarry = c }, regPC = regPC p + 1 }
executeOperation RAL (Computer o m p) =
  let c = cpuFlag $ regA p .&. 0x80 `shift` (-7) in
  let a = (regA p `shift` 1) .|. (flagCode $ fCarry $ psw p) in
  Computer o m $ p { regA = a, psw = (psw p) { fCarry = c }, regPC = regPC p + 1 }
executeOperation RAR (Computer o m p) =
  let c = cpuFlag $ regA p .&. 0x01 in
  let a = (regA p `shift` (-1)) .|. ((flagCode $ fCarry $ psw p) `shift` 7) in
  Computer o m $ p { regA = a, psw = (psw p) { fCarry = c }, regPC = regPC p + 1 }
executeOperation DAA (Computer o m p) =
  let ac' = fAuxCarry $ psw p in
  let c' = fCarry $ psw p in
  let l' = (if ac' then 0x10 else 0x00) .|. (regA p .&. 0x0F) in
  let ac = l' > 9 in
  let l = if ac then l' - 10 else l' in
  let h1 = (if c' then 0x10 else 0x00) .|. (regA p .&. 0xF0 `shift` (-4)) in
  let h2 = if ac' then h1 - 1 else h1 in
  let h' = if ac then h2 + 1 else h2 in
  let c = h' > 9 in
  let h = if c then h' - 10 else h' in
  let a = (h `shift` 4) .|. l in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation CMA (Computer o m p) = Computer o m $ p { regA = complement (regA p), regPC = regPC p + 1 }
executeOperation STC (Computer o m p) = Computer o m $ p { psw = (psw p) { fCarry = True }, regPC = regPC p + 1 }
executeOperation CMC (Computer o m p) = Computer o m $ p { psw = (psw p) { fCarry = not $ fCarry $ psw p }, regPC = regPC p + 1 }
executeOperation RET c = executeReturn c
executeOperation RET' c = executeReturn c
executeOperation PCHL (Computer o m p) = Computer o m $ p { regPC = fromIntegral (regH p) * 256 + fromIntegral (regL p) }
executeOperation SPHL (Computer o m p) = Computer o m $ p { regSP = fromIntegral (regH p) * 256 + fromIntegral (regL p), regPC = regPC p + 1 }
executeOperation JMP c = executeJump c
executeOperation JMP' c = executeJump c
executeOperation OUT (Computer o m p) =
  let n = loadByte m p in
  let o' = o // [(fromIntegral n, regA p)] in
  Computer o' m $ p { regPC = regPC p + 2 }
executeOperation IN (Computer o m p) =
  let n = loadByte m p in
  let b = fromMaybe 0 $ o !? (fromIntegral n) in
  Computer o m $ p { regA = b, regPC = regPC p + 2 }
executeOperation XTHL (Computer o m p) =
  let l = regL p in
  let h = regH p in
  let l' = fromMaybe 0 $ m !? (fromIntegral $ regSP p) in
  let h' = fromMaybe 0 $ m !? (fromIntegral $ regSP p + 1) in
  let m' = m // [(fromIntegral $ regSP p, l), (fromIntegral $ regSP p + 1, h)] in
  Computer o m' $ p { regH = h', regL = l', regPC = regPC p + 1 }
executeOperation XCHG (Computer o m p) =
  let l = regL p in
  let h = regH p in
  let e = regE p in
  let d = regD p in
  Computer o m $ p { regH = d, regL = e, regD = h, regE = l, regPC = regPC p + 1 }
executeOperation DI (Computer o m p) = Computer o m $ p { interruptsEnabled = False, regPC = regPC p + 1 }
executeOperation EI (Computer o m p) = Computer o m $ p { interruptsEnabled = True, regPC = regPC p + 1 }
executeOperation ADI (Computer o m p) =
  let d = loadByte m p in
  let ac = (regA p .&. 0x0F) + (d .&. 0x0F) > 0x0F in
  let r = fromIntegral (regA p) + fromIntegral d in
  let c = (r :: Word16) > 0xFF in
  let a = fromIntegral $ r .&. 0xFF in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 2 }
executeOperation ACI (Computer o m p) =
  let d = fromIntegral (loadByte m p) + fromIntegral (flagCode $ fCarry $ psw p) in
  let ac = (fromIntegral (regA p) .&. 0x0F) + (d .&. 0x0F) > 0x0F in
  let r = fromIntegral (regA p) + d in
  let c = (r :: Word16) > 0xFF in
  let a = fromIntegral $ r .&. 0xFF in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 2 }
executeOperation SUI (Computer o m p) =
  let d = loadByte m p in
  let ac = (regA p .&. 0x0F) < (d .&. 0x0F) in
  let c = regA p < d in
  let r = if c then 0x0100 .|. fromIntegral (regA p) else fromIntegral (regA p) in
  let a = fromIntegral $ (r :: Word16) - fromIntegral d in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 2 }
executeOperation SBI (Computer o m p) =
  let d = fromIntegral (loadByte m p) + fromIntegral (flagCode $ fCarry $ psw p) in
  let ac = (fromIntegral (regA p) .&. 0x0F) < (d .&. 0x0F) in
  let c = fromIntegral (regA p) < (d :: Word16) in
  let r = if c then 0x0100 .|. fromIntegral (regA p) else fromIntegral (regA p) in
  let a = fromIntegral $ r - d in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 2 }
executeOperation ANI (Computer o m p) =
  let d = loadByte m p in
  let a = regA p .&. d in
  let s = (pswUpdate (psw p) a) { fCarry = False, fAuxCarry = False } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 2 }
executeOperation XRI (Computer o m p) =
  let d = loadByte m p in
  let a = regA p `xor` d in
  let s = (pswUpdate (psw p) a) { fCarry = False, fAuxCarry = False } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 2 }
executeOperation ORI (Computer o m p) =
  let d = loadByte m p in
  let a = regA p .|. d in
  let s = (pswUpdate (psw p) a) { fCarry = False, fAuxCarry = False } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 2 }
executeOperation CPI (Computer o m p) =
  let d = loadByte m p in
  let z = regA p == d in
  let ac = (regA p .&. 0x0F) < (d .&. 0x0F) in
  let c = regA p < d in
  let r = (regA p .&. 0x01) == (d .&. 0x01) in
  let s = (psw p) { fZero = z, fSign = c, fCarry = c, fAuxCarry = ac, fParity = r } in
  Computer o m $ p { psw = s, regPC = regPC p + 2 }
executeOperation (MOV R_M R_M) (Computer o m p) = Computer o m $ p { isHalted = True }
executeOperation (MOV rd rs) (Computer o m p) =
  let d = getReg rs m p in
  setReg rd d $ Computer o m $ p { regPC = regPC p + 1 }
executeOperation (ADD rs) (Computer o m p) =
  let d = getReg rs m p in
  let ac = (regA p .&. 0x0F) + (d .&. 0x0F) > 0x0F in
  let r = fromIntegral (regA p) + fromIntegral d in
  let c = (r :: Word16) > 0xFF in
  let a = fromIntegral $ r .&. 0xFF in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation (ADC rs) (Computer o m p) =
  let d = fromIntegral (getReg rs m p) + fromIntegral (flagCode $ fCarry $ psw p) in
  let ac = (fromIntegral (regA p) .&. 0x0F) + (d .&. 0x0F) > 0x0F in
  let r = fromIntegral (regA p) + d in
  let c = (r :: Word16) > 0xFF in
  let a = fromIntegral $ r .&. 0xFF in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation (SUB rs) (Computer o m p) =
  let d = getReg rs m p in
  let ac = (regA p .&. 0x0F) < (d .&. 0x0F) in
  let c = regA p < d in
  let r = if c then 0x0100 .|. fromIntegral (regA p) else fromIntegral (regA p) in
  let a = fromIntegral $ (r :: Word16) - fromIntegral d in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation (SBB rs) (Computer o m p) =
  let d = fromIntegral (getReg rs m p) + fromIntegral (flagCode $ fCarry $ psw p) in
  let ac = (fromIntegral (regA p) .&. 0x0F) < (d .&. 0x0F) in
  let c = fromIntegral (regA p) < (d :: Word16) in
  let r = if c then 0x0100 .|. fromIntegral (regA p) else fromIntegral (regA p) in
  let a = fromIntegral $ r - d in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation (ANA rs) (Computer o m p) =
  let d = getReg rs m p in
  let a = regA p .&. d in
  let s = (pswUpdate (psw p) a) { fCarry = False, fAuxCarry = False } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation (XRA rs) (Computer o m p) =
  let d = getReg rs m p in
  let a = regA p `xor` d in
  let s = (pswUpdate (psw p) a) { fCarry = False, fAuxCarry = False } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation (ORA rs) (Computer o m p) =
  let d = getReg rs m p in
  let a = regA p .|. d in
  let s = (pswUpdate (psw p) a) { fCarry = False, fAuxCarry = False } in
  Computer o m $ p { regA = a, psw = s, regPC = regPC p + 1 }
executeOperation (CMP rs) (Computer o m p) =
  let d = getReg rs m p in
  let z = regA p == d in
  let ac = (regA p .&. 0x0F) < (d .&. 0x0F) in
  let c = regA p < d in
  let r = (regA p .&. 0x01) == (d .&. 0x01) in
  let s = (psw p) { fZero = z, fSign = c, fCarry = c, fAuxCarry = ac, fParity = r } in
  Computer o m $ p { psw = s, regPC = regPC p + 1 }
executeOperation (NOP _) (Computer o m p) = Computer o m $ p { regPC = regPC p + 1 }
executeOperation (LXI rd) (Computer o m p) =
  let d = loadWord m p in
  let p' = setRegPair rd d p in
  Computer o m $ p' { regPC = regPC p + 3 }
executeOperation (DAD rs) (Computer o m p) =
  let d = getRegPair rs p in
  let w = fromIntegral (regH p) * 256 + fromIntegral (regL p) in
  let r = fromIntegral (w :: Word16) + fromIntegral d in
  let c = (r :: Word32) > 0xFFFF in
  let h' = fromIntegral $ (r .&. 0xFF00) `shift` (-8) in
  let l' = fromIntegral $ r .&. 0xFF in
  Computer o m $ p { psw = (psw p) { fCarry = c }, regH = h', regL = l', regPC = regPC p + 1 }
executeOperation (STAX rs) (Computer o m p) =
  let addr = getRegExt rs p in
  let m' = m // [(fromIntegral addr, regA p)] in
  Computer o m' $ p { regPC = regPC p + 1 }
executeOperation (LDAX rd) (Computer o m p) =
  let addr = getRegExt rd p in
  let d = fromMaybe 0 $ m !? fromIntegral addr in
  Computer o m $ p { regA = d, regPC = regPC p + 1 }
executeOperation (INX rd) (Computer o m p) =
  let d = getRegPair rd p in
  let p' = setRegPair rd (d + 1) p in
  Computer o m $ p' { regPC = regPC p + 1 }
executeOperation (DCX rd) (Computer o m p) =
  let d = getRegPair rd p in
  let p' = setRegPair rd (d - 1) p in
  Computer o m $ p' { regPC = regPC p + 1 }
executeOperation (INR rd) (Computer o m p) =
  let d = getReg rd m p in
  let ac = (d .&. 0x0F) + 1 > 0x0F in
  let r = fromIntegral d + 1 in
  let c = (r :: Word16) > 0xFF in
  let a = fromIntegral $ r .&. 0xFF in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  setReg rd a $ Computer o m $ p { psw = s, regPC = regPC p + 1 }
executeOperation (DCR rd) (Computer o m p) =
  let d = getReg rd m p in
  let ac = (d .&. 0x0F) < 1 in
  let c = d < 1 in
  let r = if c then 0x0100 .|. fromIntegral d else fromIntegral d in
  let a = fromIntegral $ (r :: Word16) - 1 in
  let s = (pswUpdate (psw p) a) { fCarry = c, fAuxCarry = ac } in
  setReg rd a $ Computer o m $ p { psw = s, regPC = regPC p + 1 }
executeOperation (MVI rd) (Computer o m p) =
  let d = loadByte m p in
  setReg rd d $ Computer o m $ p { regPC = regPC p + 2 }
executeOperation (RCC cond) (Computer o m p) =
  if fitCondition cond (psw p)
    then executeReturn (Computer o m p)
    else Computer o m $ p { regPC = regPC p + 1 }
executeOperation (POP rd) (Computer o m p) =
  let l = fromMaybe 0 $ m !? (fromIntegral $ regSP p) in
  let h = fromMaybe 0 $ m !? (fromIntegral $ regSP p + 1) in
  let p' = setRegWord rd (fromIntegral h * 256 + fromIntegral l) p in
  Computer o m $ p' { regSP = regSP p + 2, regPC = regPC p + 1 }
executeOperation (JCC cond) (Computer o m p) =
  if fitCondition cond (psw p)
    then executeJump (Computer o m p)
    else Computer o m $ p { regPC = regPC p + 3 }
executeOperation (CCC cond) (Computer o m p) =
  if fitCondition cond (psw p)
    then executeCall (Computer o m p)
    else Computer o m $ p { regPC = regPC p + 3 }
executeOperation (PUSH rs) (Computer o m p) =
  let d = getRegWord rs p in
  let h = fromIntegral $ (d .&. 0xFF00) `shift` (-8) in
  let l = fromIntegral $ d .&. 0x00FF in
  let m' = m // [(fromIntegral (regSP p - 1), h), (fromIntegral (regSP p - 2), l)] in
  Computer o m' $ p { regSP = regSP p - 2, regPC = regPC p + 1 }
executeOperation (CALL _) c = executeCall c
executeOperation (RST n) (Computer o m p) = Computer o m $ p { regPC = fromIntegral n * 8 }

cpuStep :: Computer -> Computer
cpuStep (Computer o m p) =
  let op_code = fromMaybe 0 $ m !? (fromIntegral $ regPC p) in
  let op = cpuOperation op_code in
  executeOperation op (Computer o m p { timer = timer p + operationTicks op (psw p) })

hl :: Word8 -> Word8 -> Word16
hl h l = (fromIntegral h `shift` 8) .|. fromIntegral l

getTicks :: Computer -> Int
getTicks = timer . cpu

getPort :: Word8 -> Computer -> Word8
getPort n (Computer o _ _) = fromMaybe 0 $ o !? fromIntegral n

setPort :: Word8 -> Word8 -> Computer -> Computer
setPort n a (Computer o m p) = Computer (o // [(fromIntegral n, a)]) m p

getMemory :: Word16 -> Computer -> Word8
getMemory addr (Computer _ m _) = fromMaybe 0 $ m !? fromIntegral addr

setMemory :: Word16 -> Word8 -> Computer -> Computer
setMemory addr a (Computer o m p) = Computer o (m // [(fromIntegral addr, a)]) p

isCPUHalted :: Computer -> Bool
isCPUHalted = isHalted . cpu

areInterruptsEnabled :: Computer -> Bool
areInterruptsEnabled = interruptsEnabled . cpu

getPC :: Computer -> Word16
getPC = regPC . cpu

setPC :: Word16 -> Computer -> Computer
setPC w c = c { cpu = (cpu c) { regPC = w } }

getRegister :: CPURegister -> Computer -> Word8
getRegister r c = getReg r (memory c) (cpu c)

getFlag :: CPUCondition -> Computer -> Bool
getFlag cond c = fitCondition cond (psw $ cpu c)

newtype Program = Program [(Word8, Word8, Word8, String)]

setProgram :: Program -> Computer -> Computer
setProgram (Program program) computer = foldl (\c (address_h, address_l, opcode, _) -> setMemory (hl address_h address_l) opcode c) computer program

showByte :: Word8 -> String
showByte b
  | b < 8 = "00" ++ showOct b ""
  | b < 64 = "0" ++ showOct b ""
  | otherwise = showOct b ""

instance Show Program where
  show (Program p) = intercalate "\n" [showByte h ++ " " ++ showByte l ++ " " ++ showByte c ++ " " ++ t | (h, l, c, t) <- p]
