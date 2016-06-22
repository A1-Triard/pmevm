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

pswCode :: PSW -> Word8
pswCode p
  = (flagCode $ fCarry p)
  + 1 * 2
  + (flagCode $ fParity p) * 4
  + (flagCode $ fAuxCarry p) * 16
  + (flagCode $ fZero p) * 64
  + (flagCode $ fSign p) * 128

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
data CPURegisterWord = W_BC | W_DE | W_HL | W_APSW deriving (Eq, Show, Ord, Enum)

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
registerWordCode W_APSW = 6

cpuRegisterWord :: Word8 -> CPURegisterWord
cpuRegisterWord 0 = W_BC
cpuRegisterWord 2 = W_DE
cpuRegisterWord 4 = W_HL
cpuRegisterWord 6 = W_APSW
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
  | HLT
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

cycles :: CPUOperation -> PSW -> Int
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
cycles (RCC f) p = if fitCondition f p then 11 else 5
cycles RET _ = 10
cycles RET' _ = 10
cycles RLC _ = 4
cycles RRC _ = 4
cycles RAL _ = 4
cycles RAR _ = 4
cycles XCHG _ = 4
cycles XTHL _ = 18
cycles SPHL _ = 5
cycles PCHL _ = 5
cycles HLT _ = 7
cycles (NOP _) _ = 4
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
cycles JMP' _ = 10
cycles (CCC f) p = if fitCondition f p then 17 else 11
cycles (CALL _) _ = 17
cycles (LXI _) _ = 10
cycles STA _ = 13
cycles LDA _ = 13
cycles SHLD _ = 16
cycles LHLD _ = 16

fromOctets :: Word8 -> Word8 -> Word8 -> Word8
fromOctets a b c = a * 64 + b * 8 + c

operationCode :: CPUOperation -> Word8
operationCode HLT = 0o166
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
cpuOperation 0o166 = HLT
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
