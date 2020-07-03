import           Test.HUnit
import qualified TestMath
import qualified TestMesh

main :: IO Counts
main
  = runTestTT $ TestList
    [ TestMath.tests
    , TestMesh.tests
    ]
