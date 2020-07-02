module Mesh (Verticies, Edges, Mesh(..), saveMesh, loadMesh) where

import Math

type Verticies
  = [Vector]

type Edges
  = [(Int, Int)]

data Mesh
  = Mesh Verticies Edges

saveMesh :: Mesh -> IO ()
saveMesh (Mesh verticies edges)
  = undefined

loadMesh :: String -> IO Mesh
loadMesh filename
  = undefined
