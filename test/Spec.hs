import           Test.HUnit
import qualified TestMath

main :: IO Counts
main
  = runTestTT $ TestList
    [ TestMath.tests
    ]
