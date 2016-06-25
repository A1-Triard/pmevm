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
  [ TestCase freeKeyboardTest
  , TestCase sklTest
  ]
  
freeKeyboardTest :: Assertion
freeKeyboardTest = do
  assertEqual "" 15 $ getPortIn 3 $ keyboardStep initKeyboard $ setPortOut 3 0 $ initComputer

wait :: Int -> Keyboard -> Computer -> Computer
wait n k c = fromMaybe (error "") $ find (\x -> getTicks x >= getTicks c + n) $ iterate (keyboardStep k . cpuStep) c

{-
steps :: Int -> Keyboard -> Computer -> Computer
steps n k c = fromMaybe (error "") $ find (const True) $ drop n $ iterate (keyboardStep k . cpuStep) c
-}

sklTest :: Assertion
sklTest = do
  let k0 = initKeyboard
  let k1 = initKeyboard { key0 = True }
  let c0 = setPC (hl 0o014 0o000) $ setProgram testProgram $ setProgram monitor initComputer
  let c1 = wait 100000 k0 c0
  assertEqual "" 0 $ getPortOut 1 c1
  let c2 = wait 100000 k1 c1
  assertEqual "" 7 $ getPortOut 1 c2
  let c3 = wait 100000 k0 c2
  assertEqual "" 7 $ getPortOut 1 c3

{-
sklStepByStepTest :: Assertion
sklStepByStepTest = do
  let k0 = initKeyboard
  let k1 = initKeyboard { key2 = True }
  let c0 = setPC (hl 0o014 0o000) $ setProgram testProgram $ setProgram monitor initComputer
  let c1 = steps 4 k0 c0
  assertEqual "" (hl 0o000 0o205) $ getPC c1
  assertEqual "" 15 $ getRegister R_A c1
  let c2 = steps 1 k0 c1
  assertEqual "" (hl 0o000 0o207) $ getPC c2
  assertEqual "" 15 $ getRegister R_A c2
  let c3 = steps 2 k0 c2
  assertEqual "" (hl 0o000 0o214) $ getPC c3
  let c4 = steps 2111 k0 c3
  assertEqual "" (hl 0o000 0o217) $ getPC c4
  let c5 = steps 7 k0 c4
  assertEqual "" (hl 0o000 0o231) $ getPC c5
  assertEqual "" 15 $ getPort 3 c5
  let c6 = steps 4 k0 c5
  assertEqual "" (hl 0o000 0o242) $ getPC c6
  let c7 = steps 16 k0 c6
  assertEqual "" (hl 0o000 0o224) $ getPC c7
  let c8 = steps 23 k0 c7
  assertEqual "" (hl 0o000 0o246) $ getPC c8
  let c9 = steps 27 k0 c8
  assertEqual "" (hl 0o000 0o246) $ getPC c9
  let c10 = steps 26 k0 c9
  assertEqual "" (hl 0o000 0o220) $ getPC c10
  let c11 = steps 3 k1 c10
  assertEqual "" (hl 0o000 0o225) $ getPC c11
  assertEqual "" 0o376 $ getRegister R_A c11
  let c11' = cpuStep c11
  assertEqual "" 0o376 $ getPort 3 c11'
  let c11'' = keyboardStep k1 c11'
  assertEqual "" 13 $ getPort 3 c11''
  let c12 = steps 1 k1 c11
  assertEqual "" (hl 0o000 0o227) $ getPC c12
  assertEqual "" 13 $ getPort 3 c12
  let c13 = steps 1 k1 c12
  assertEqual "" (hl 0o000 0o230) $ getPC c13
  assertEqual "" 13 $ getPort 3 c13
  let c14 = steps 1 k1 c13
  assertEqual "" (hl 0o000 0o231) $ getPC c14
  assertEqual "" 13 $ getPort 3 c14
  let c15 = steps 1 k1 c14
  assertEqual "" (hl 0o000 0o233) $ getPC c15
  assertEqual "" 15 $ getRegister R_A c15
-}

testProgram :: Program
testProgram = Program
  [ (0o014, 0o000, 0o315, "   START: CALL SKL          ")
  , (0o014, 0o001, 0o177, "                            ")
  , (0o014, 0o002, 0o000, "                            ")
  , (0o014, 0o003, 0o376, "          CPI 177Q          ")
  , (0o014, 0o004, 0o177, "                            ")
  , (0o014, 0o005, 0o312, "          JZ START          ")
  , (0o014, 0o006, 0o000, "                            ")
  , (0o014, 0o007, 0o014, "                            ")
  , (0o014, 0o010, 0o323, "          OUT 001Q          ")
  , (0o014, 0o011, 0o001, "                            ")
  , (0o014, 0o012, 0o303, "          JMP START         ")
  , (0o014, 0o013, 0o000, "                            ")
  , (0o014, 0o014, 0o014, "                            ")
  ]
