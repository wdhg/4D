module TestMath (tests) where

import Test.HUnit
import Math

vector1, vector2, vector3, vector4, vector5 :: Vector
vector1 = Vector [1,2,3]
vector2 = Vector [4,5,6]
vector3 = Vector [2,3,4,5]
vector4 = Vector [5,7,9]
vector5 = Vector [3,5,7,5]

addTests :: Test
addTests
  = TestList
  [ add vector1 vector2 ~?= vector4
  , add vector1 vector3 ~?= vector5
  ]

tests :: Test
tests
  = TestList
    [ "addTest" ~: addTest
    ]

