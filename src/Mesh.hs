module Mesh
  ( Verticies, Edges, Mesh(..)
  , marshalMesh, unmarshalMesh, saveMesh, loadMesh
  ) where

import Math

type Verticies
  = [Vector]

type Edges
  = [(Int, Int)]

data Mesh
  = Mesh Verticies Edges

marshalMesh :: Mesh -> String
marshalMesh mesh
  = undefined

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
