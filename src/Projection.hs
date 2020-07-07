module Projection where

import Math

data Camera
  = Camera Position Rotation FOV FocalLength

type Position
  = Vector

type Rotation
  = Vector

type FOV
  = Double

type FocalLength
  = Double

screenIntersection :: Camera -> Position -> Position
screenIntersection (Camera c n fov a) x
  = add (scale (a / (horizontalProjection (Camera c n fov a) x)) (sub x c)) c

horizontalProjection :: Camera -> Position -> Double
horizontalProjection (Camera c n fov a) x
  = (magnitude (sub x c)) / (findAngle (Camera c n fov a) x)

findAngle :: Camera -> Position -> Double
findAngle (Camera c n fov a) x
  = (dot n (sub x c)) / magnitude (sub x c)

dot :: Vector -> Vector -> Double
dot
  = undefined

magnitude :: Vector -> Double
magnitude
  = undefined

