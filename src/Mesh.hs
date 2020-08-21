module Mesh
  ( Verticies, Edges, Mesh(..)
  , tesseract, marshalMesh, unmarshalMesh, saveMesh, loadMesh
  ) where

import Control.Monad   (liftM2)
import Data.List.Split (splitOn)
import Data.Maybe      (isNothing, listToMaybe)
import Math

type Verticies
  = [Vector]

type Edge
  = (Int, Int)

type Edges
  = [Edge]

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

marshalEdge :: Edge -> String
marshalEdge (index1, index2)
  = show index1 ++ (' ' : show index2)

marshalMesh :: Mesh -> String
marshalMesh (Mesh vertices edges)
  = verticesText ++ ('\n' : edgesText)
    where
      verticesText
        = commaJoin $ map marshalVertex vertices
      edgesText
        = commaJoin $ map marshalEdge edges

readMaybe :: Read a => String -> Maybe a
readMaybe
  = fmap fst . listToMaybe . reads

unmarshalCommaSplit :: (String -> Maybe b) -> String -> Maybe [b]
unmarshalCommaSplit func
  = sequence . map func . splitOn ","

unmarshalVertex :: String -> Maybe Vector
unmarshalVertex
  = fmap Vector . sequence . map readMaybe . words

unmarshalVertices :: String -> Maybe [Vector]
unmarshalVertices
  = unmarshalCommaSplit unmarshalVertex

unmarshalEdge :: String -> Maybe Edge
unmarshalEdge text
  = case map readMaybe $ words text of
      [maybeX, maybeY] -> liftM2 (,) maybeX maybeY
      _                -> Nothing

unmarshalEdges :: String -> Maybe Edges
unmarshalEdges
  = unmarshalCommaSplit unmarshalEdge

unmarshalMesh :: String -> Maybe Mesh
unmarshalMesh text
  = case splitOn "\n" text of
      [verticesText, edgesText]
        -> do
          vertices <- unmarshalVertices verticesText
          edges <- unmarshalEdges edgesText
          return $ Mesh vertices edges
      _                         -> Nothing

saveMesh :: String -> Mesh -> IO ()
saveMesh filename mesh
  = writeFile filename (marshalMesh mesh)

loadMesh :: String -> IO (Maybe Mesh)
loadMesh filename
  = do
    text <- readFile filename
    return $ unmarshalMesh text
