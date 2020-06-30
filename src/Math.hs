class Nums a where
  add :: a -> a -> a
  sub :: a -> a -> a
  scale :: Double -> a -> a
  neg :: a -> a


newtype Vector
  = Vector [Double]

newtype Matrix
  = Matrix [Vector]

