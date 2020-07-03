module Mesh
  ( Verticies, Edges, Mesh(..)
  , tesseract, marshalMesh, unmarshalMesh, saveMesh, loadMesh
  ) where

import Math

type Verticies
  = [Vector]

type Edges
  = [(Int, Int)]

data Mesh
  = Mesh Verticies Edges

tesseract :: Mesh
tesseract
  = Mesh
    [Vector [x, y, z, w] | w <- [0,1], z <- [0,1], y <- [0,1], x <- [0,1]]
    [ (0,1), (1,3), (3,2), (2,0)
    , (4,5), (5,7), (7,6), (6,4)
    , (0,4), (1,5), (3,7), (2,6)
    , (8,9), (9,11), (11,10), (10,8)
    , (12,13), (13,15), (15,14), (14,12)
    , (8,12), (9,13), (11,15), (10,14)
    , (0,8), (1,9), (2,11), (3,12)
    , (4,12), (5,13), (6,14), (7,15)
    ]

commaJoin :: [String] -> String
commaJoin (x:xs)
  = x ++ (concatMap (", " ++) xs)

marshalVertex :: Vector -> String
marshalVertex (Vector xs)
  = unwords $ map show xs

marshalEdge :: (Int, Int) -> String
marshalEdge (index1, index2)
  = show index1 ++ (' ' : show index2)

marshalMesh :: Mesh -> String
marshalMesh (Mesh vertices edges)
  = verticesText ++ ('\n' : edgesText)
    where
      verticesText
        = commaJoin $ map marshalVertex vertices
      edgesText
        = unwords $ map marshalEdge edges

unmarshalMesh :: String -> Mesh
unmarshalMesh text
  = undefined

saveMesh :: String -> Mesh -> IO ()
saveMesh filename mesh
  = writeFile filename (marshalMesh mesh)

loadMesh :: String -> IO Mesh
loadMesh filename
  = do
    text <- readFile filename
    return $ unmarshalMesh text
