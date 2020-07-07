module Projection where

data Camera
  = Camera Position Rotation FOV FocalLength

type Position
  = (Double, Double, Double)

type Rotation
  = Vector

type FOV
  = Double

type FocalLength
  = Double


