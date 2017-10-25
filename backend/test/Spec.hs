#define TESTS
#include <haskell>
import qualified Data.Pmevm.Spec
import qualified Data.Pmevm.Keyboard.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Data.Pmevm.Spec.tests
  , Data.Pmevm.Keyboard.Spec.tests
  ]
