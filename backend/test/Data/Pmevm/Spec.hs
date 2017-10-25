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
  assertEqual "" 0 $ getPortOut 2 c0
  let c1 = cpuStep c0
  assertEqual "" 0 $ getPortOut 2 c1
  let c2 = cpuStep c1
  assertEqual "" 1 $ getPortOut 2 c2

jccTest :: Assertion
jccTest = do
  let
    c0 =
      setPC (hl 0o014 0o000) $
      setMemory (hl 0o014 0o000) (operationCode $ JCC C_NZ) $
      setMemory (hl 0o014 0o001) 0o023 $
      setMemory (hl 0o014 0o002) 0o014 $
      initComputer
  let c1 = cpuStep c0
  assertEqual "" (hl 0o014 0o023) $ getPC c1

sumNumbersShortProgramStepByStepTest :: Assertion
sumNumbersShortProgramStepByStepTest = do
  let c0 = setPC (hl 0o014 0o000) $ setProgram sumNumbersShortProgram initComputer
  assertEqual "1" 0 $ getPortOut 0 c0
  assertEqual "2" (hl 0o014 0o000) $ getPC c0
  let c1 = cpuStep c0
  assertEqual "3" (hl 0o014 0o002) $ getPC c1
  let c2 = cpuStep c1
  assertEqual "4" (hl 0o014 0o004) $ getPC c2
  assertEqual "5" 0o002 $ getRegister R_D c2
  let c3 = cpuStep c2
  assertEqual "6" (hl 0o014 0o005) $ getPC c3
  assertEqual "7" 0o002 $ getRegister R_A c3
  let c4 = cpuStep c3
  assertEqual "8" (hl 0o014 0o006) $ getPC c4
  assertEqual "9" 0o001 $ getRegister R_D c4
  assertEqual "10" True $ getFlag C_NZ c4
  let c5 = cpuStep c4
  assertEqual "11" (hl 0o014 0o004) $ getPC c5

sumNumbersProgramStepByStepTest :: Assertion
sumNumbersProgramStepByStepTest = do
  let c0 = setPC (hl 0o014 0o000) $ setProgram sumNumbersProgram initComputer
  assertEqual "" 0 $ getPortOut 0 c0
  let c = take 100 $ iterate cpuStep c0
  let ch = fromMaybe initComputer $ find isCPUHalted c
  assertEqual "" 210 $ getPortOut 0 ch

sumNumbersProgram :: Program
sumNumbersProgram = Program
  [ (0o014, 0o000, 0o076, "          MVI A 000Q        ")
  , (0o014, 0o001, 0o000, "                            ")
  , (0o014, 0o002, 0o026, "          MVI D 024Q        ")
  , (0o014, 0o003, 0o024, "                            ")
  , (0o014, 0o004, 0o202, "      M1: ADD D             ")
  , (0o014, 0o005, 0o025, "          DCR D             ")
  , (0o014, 0o006, 0o302, "          JNZ M1            ")
  , (0o014, 0o007, 0o004, "                            ")
  , (0o014, 0o010, 0o014, "                            ")
  , (0o014, 0o011, 0o323, "          OUT 000Q          ")
  , (0o014, 0o012, 0o000, "                            ")
  , (0o014, 0o013, 0o166, "          HLT               ")
  ]

sumNumbersShortProgram :: Program
sumNumbersShortProgram = Program
  [ (0o014, 0o000, 0o076, "          MVI A 000Q        ")
  , (0o014, 0o001, 0o000, "                            ")
  , (0o014, 0o002, 0o026, "          MVI D 002Q        ")
  , (0o014, 0o003, 0o002, "                            ")
  , (0o014, 0o004, 0o202, "      M1: ADD D             ")
  , (0o014, 0o005, 0o025, "          DCR D             ")
  , (0o014, 0o006, 0o302, "          JNZ M1            ")
  , (0o014, 0o007, 0o004, "                            ")
  , (0o014, 0o010, 0o014, "                            ")
  , (0o014, 0o011, 0o323, "          OUT 000Q          ")
  , (0o014, 0o012, 0o000, "                            ")
  , (0o014, 0o013, 0o166, "          HLT               ")
  ]
