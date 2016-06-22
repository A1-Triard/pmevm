module Data.Pmevm.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>
import Data.Pmevm

tests :: Test
tests = TestList
  [ opCodesTest
  ]

opCodesTest :: Test
opCodesTest = TestList [TestCase (opCodesCase op_code) | op_code <- [minBound .. maxBound]]
  
opCodesCase :: Word8 -> Assertion
opCodesCase op_code = do
  assertEqual (show op_code) op_code $ operationCode $ cpuOperation op_code
