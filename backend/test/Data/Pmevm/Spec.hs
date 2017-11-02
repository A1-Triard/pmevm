--
-- Copyright 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Data.Pmevm.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Pmevm

tests :: Test
tests = TestList
  [ opCodesTest
  , TestCase simpleProgramStepByStepTest
  , TestCase jccTest
  , TestCase sumNumbersShortProgramStepByStepTest
  , TestCase sumNumbersProgramStepByStepTest
  ]

opCodesTest :: Test
opCodesTest = TestList [TestCase (opCodesCase op_code) | op_code <- [minBound .. maxBound]]

opCodesCase :: Word8 -> Assertion
opCodesCase op_code = do
  assertEqual (show op_code) op_code $ operationCode $ cpuOperation op_code

simpleProgramStepByStepTest :: Assertion
simpleProgramStepByStepTest = do
  let c0 = setMemory 2 2 $ setMemory 1 (operationCode OUT) $ setMemory 0 (operationCode $ INR R_A) $ initComputer
  let (c1, o1, t1) = cpuStep (const 0) c0
  assertEqual "" Nothing o1
  assertEqual "" 5 t1
  let (_, o2, t2) = cpuStep (const 0) c1
  assertEqual "" (Just $ PortOut 2 1) o2
  assertEqual "" 10 t2

jccTest :: Assertion
jccTest = do
  let
    c0 =
      setPC (hl 0o014 0o000) $
      setMemory (hl 0o014 0o000) (operationCode $ JCC C_NZ) $
      setMemory (hl 0o014 0o001) 0o023 $
      setMemory (hl 0o014 0o002) 0o014 $
      initComputer
  let (c1, o1, t1) = cpuStep (const 0) c0
  assertEqual "" (hl 0o014 0o023) $ getPC c1
  assertEqual "" Nothing o1
  assertEqual "" 10 t1

sumNumbersShortProgramStepByStepTest :: Assertion
sumNumbersShortProgramStepByStepTest = do
  let c0 = setPC (hl 0o014 0o000) $ setProgram sumNumbersShortProgram initComputer
  assertEqual "2" (hl 0o014 0o000) $ getPC c0
  let (c1, o1, t1) = cpuStep (const 0) c0
  assertEqual "3" (hl 0o014 0o002) $ getPC c1
  assertEqual "" Nothing o1
  assertEqual "" 7 t1
  let (c2, o2, t2) = cpuStep (const 0) c1
  assertEqual "4" (hl 0o014 0o004) $ getPC c2
  assertEqual "5" 0o002 $ getRegister R_D c2
  assertEqual "" Nothing o2
  assertEqual "" 7 t2
  let (c3, o3, t3) = cpuStep (const 0) c2
  assertEqual "6" (hl 0o014 0o005) $ getPC c3
  assertEqual "7" 0o002 $ getRegister R_A c3
  assertEqual "" Nothing o3
  assertEqual "" 4 t3
  let (c4, o4, t4) = cpuStep (const 0) c3
  assertEqual "8" (hl 0o014 0o006) $ getPC c4
  assertEqual "9" 0o001 $ getRegister R_D c4
  assertEqual "10" True $ getFlag C_NZ c4
  assertEqual "" Nothing o4
  assertEqual "" 5 t4
  let (c5, o5, t5) = cpuStep (const 0) c4
  assertEqual "11" (hl 0o014 0o004) $ getPC c5
  assertEqual "" Nothing o5
  assertEqual "" 10 t5

port0Step :: (Computer, Word8) -> ((Computer, Word8), Int64)
port0Step c =
  let (cn, po, t) = cpuStep (const 0) (fst c) in
  case po of
    Just (PortOut 0 v) -> ((cn, v), t)
    _ -> ((cn, snd c), t)

sumNumbersProgramStepByStepTest :: Assertion
sumNumbersProgramStepByStepTest = do
  let c0 = setPC (hl 0o014 0o000) $ setProgram sumNumbersProgram initComputer
  let (c, o) = fromMaybe (error "") $ listToMaybe $ drop 100 $ iterate (fst . port0Step) (c0, 0)
  assertBool "" $ isCpuHalted c
  assertEqual "" 210 o

sumNumbersProgram :: Program
sumNumbersProgram = Program
  [ I 0o014 0o000 0o076 "          MVI A 000Q        "
  , I 0o014 0o001 0o000 "                            "
  , I 0o014 0o002 0o026 "          MVI D 024Q        "
  , I 0o014 0o003 0o024 "                            "
  , I 0o014 0o004 0o202 "      M1: ADD D             "
  , I 0o014 0o005 0o025 "          DCR D             "
  , I 0o014 0o006 0o302 "          JNZ M1            "
  , I 0o014 0o007 0o004 "                            "
  , I 0o014 0o010 0o014 "                            "
  , I 0o014 0o011 0o323 "          OUT 000Q          "
  , I 0o014 0o012 0o000 "                            "
  , I 0o014 0o013 0o166 "          HLT               "
  ]

sumNumbersShortProgram :: Program
sumNumbersShortProgram = Program
  [ I 0o014 0o000 0o076 "          MVI A 000Q        "
  , I 0o014 0o001 0o000 "                            "
  , I 0o014 0o002 0o026 "          MVI D 002Q        "
  , I 0o014 0o003 0o002 "                            "
  , I 0o014 0o004 0o202 "      M1: ADD D             "
  , I 0o014 0o005 0o025 "          DCR D             "
  , I 0o014 0o006 0o302 "          JNZ M1            "
  , I 0o014 0o007 0o004 "                            "
  , I 0o014 0o010 0o014 "                            "
  , I 0o014 0o011 0o323 "          OUT 000Q          "
  , I 0o014 0o012 0o000 "                            "
  , I 0o014 0o013 0o166 "          HLT               "
  ]
