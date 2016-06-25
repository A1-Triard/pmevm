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
  ]

opCodesTest :: Test
opCodesTest = TestList [TestCase (opCodesCase op_code) | op_code <- [minBound .. maxBound]]
  
opCodesCase :: Word8 -> Assertion
opCodesCase op_code = do
  assertEqual (show op_code) op_code $ operationCode $ cpuOperation op_code

simpleProgramStepByStepTest :: Assertion
simpleProgramStepByStepTest = do
  let c0 = setMemory 2 2 $ setMemory 1 (operationCode $ OUT) $ setMemory 0 (operationCode $ INR R_A) $ initComputer
  assertEqual "" 0 $ getPort 2 c0
  let c1 = cpuStep c0
  assertEqual "" 0 $ getPort 2 c1
  let c2 = cpuStep c1
  assertEqual "" 1 $ getPort 2 c2
