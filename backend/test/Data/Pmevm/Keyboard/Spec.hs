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

module Data.Pmevm.Keyboard.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Pmevm
import Data.Pmevm.Keyboard
import Data.Pmevm.Monitor

tests :: Test
tests = TestList
  [ TestCase sklTest
  ]

computerStep :: Keyboard -> (ComputerWithPorts, Int64) -> (ComputerWithPorts, Int64)
computerStep k (c, t) =
  let (cn, d) = keyboardStep k c in
  (cn, t + d)

wait :: Int64 -> Keyboard -> ComputerWithPorts -> ComputerWithPorts
wait n k c = fst $ fromMaybe (error "") $ find ((>= n) . snd) $ iterate (computerStep k) (c, 0)

sklTest :: Assertion
sklTest = do
  let k0 = initKeyboard
  let k1 = set key2 True initKeyboard
  let k2 = set key8 True initKeyboard
  let c0 = initPorts $ setPC (hl 0o014 0o000) $ setProgram sklTestProgram $ setProgram monitor initComputer
  let c1 = wait 100000 k0 c0
  assertEqual "" 0 $ view port1 c1
  let c2 = wait 100000 k1 c1
  assertEqual "" 2 $ view port1 c2
  let c3 = wait 100000 k0 c2
  assertEqual "" 2 $ view port1 c3
  let c4 = wait 100000 k2 c3
  assertEqual "" 8 $ view port1 c4
  let c5 = wait 100000 k0 c4
  assertEqual "" 8 $ view port1 c5

sklTestProgram :: Program
sklTestProgram = Program
  [ I 0o014 0o000 0o315 "   START: CALL SKL          "
  , I 0o014 0o001 0o177 "                            "
  , I 0o014 0o002 0o000 "                            "
  , I 0o014 0o003 0o376 "          CPI 177Q          "
  , I 0o014 0o004 0o177 "                            "
  , I 0o014 0o005 0o312 "          JZ START          "
  , I 0o014 0o006 0o000 "                            "
  , I 0o014 0o007 0o014 "                            "
  , I 0o014 0o010 0o323 "          OUT 001Q          "
  , I 0o014 0o011 0o001 "                            "
  , I 0o014 0o012 0o303 "          JMP START         "
  , I 0o014 0o013 0o000 "                            "
  , I 0o014 0o014 0o014 "                            "
  ]
