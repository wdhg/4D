module TestMath (tests) where

import Math
import Test.HUnit

vec :: [Double] -> Vector
vec = Vector

addTests :: Test
addTests
  = TestList
    [ add (vec [0,1,2]) (vec [4,5,6]) ~?= (vec [4,6,8])
    , add (vec [1,2]) (vec [1,2,3,4]) ~?= (vec [2,4,3,4])
    ]

scaleTests :: Test
scaleTests
  = TestList
    [ scale 1 (vec [1,2,3]) ~?= (vec [1,2,3])
    , scale 2 (vec [1,2,3]) ~?= (vec [2,4,6])
    , scale (-10) (vec [1,2,3]) ~?= (vec [-10,-20,-30])
    ]

negTests :: Test
negTests
  = TestList
    [ neg (vec [4,5,6]) ~?= (vec [-4,-5,-6])
    , neg (vec [1,-2,3]) ~?= (vec [-1,2,-3])
    ]

tests :: Test
tests
  = TestList
    [ "add" ~: addTests
    , "scale" ~: scaleTests
    ]
