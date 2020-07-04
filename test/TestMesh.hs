module TestMesh (tests) where

import Math
import Mesh
import Test.HUnit

marshalMeshTests :: Test
marshalMeshTests
  = TestList
    [ marshalMesh
        ( Mesh
          [Vector [0,1], Vector [1,1], Vector [1,0]]
          [(0,1), (0,2)]
        )
      ~?= "0.0 1.0, 1.0 1.0, 1.0 0.0\n0 1, 0 2"
    , marshalMesh
        ( Mesh
          [Vector [0,1,0,1], Vector [1,1], Vector [1,0]]
          [(0,1), (0,2), (1,2)]
        )
      ~?= "0.0 1.0 0.0 1.0, 1.0 1.0, 1.0 0.0\n0 1, 0 2, 1 2"
    ]

tests :: Test
tests
  = TestList
    [ marshalMeshTests
    ]
