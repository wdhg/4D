module TestMath (tests) where

import Math
import Test.HUnit

vec :: [Double] -> Vector
vec = Vector

vecAddTests :: Test
vecAddTests
  = TestList
    [ add (vec [0,1,2]) (vec [4,5,6]) ~?= (vec [4,6,8])
    , add (vec [1,2]) (vec [1,2,3,4]) ~?= (vec [2,4,3,4])
    ]

vecScaleTests :: Test
vecScaleTests
  = TestList
    [ scale 1 (vec [1,2,3]) ~?= (vec [1,2,3])
    , scale 2 (vec [1,2,3]) ~?= (vec [2,4,6])
    , scale (-10) (vec [1,2,3]) ~?= (vec [-10,-20,-30])
    ]

vecNegTests :: Test
vecNegTests
  = TestList
    [ neg (vec [4,5,6]) ~?= (vec [-4,-5,-6])
    , neg (vec [1,-2,3]) ~?= (vec [-1,2,-3])
    ]

vecSubTests :: Test
vecSubTests
  = TestList
    [ sub (vec [1,2,3]) (vec [4,5,6]) ~?= (vec [-3,-3,-3])
    , sub (vec [-1,-2,-3]) (vec [-10,-20,-30]) ~?= (vec [9, 18, 27])
    ]

matAddTests :: Test
matAddTests
  = TestList
    [ add
        (Matrix (2, 2) [(vec [1, 2]), (vec [3, 4])])
        (Matrix (2, 2) [(vec [5, 6]), (vec [7, 8])])
      ~?= (Matrix (2, 2) [(vec [6, 8]), (vec [10, 12])])
    ]

tests :: Test
tests
  = TestList
    [ "Vector add" ~: vecAddTests
    , "Vector scale" ~: vecScaleTests
    , "Vector neg" ~: vecNegTests
    , "Vector sub" ~: vecSubTests
    , "Matrix add" ~: matAddTests
    ]
