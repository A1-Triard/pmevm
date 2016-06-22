#define TESTS
#include <haskell>
import qualified Data.Pmevm.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Data.Pmevm.Spec.tests
  ]
