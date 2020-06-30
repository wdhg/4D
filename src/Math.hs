class Nums a where
  add :: a -> a -> a
  scale :: Double -> a -> a
  neg :: a -> a
  sub :: a -> a -> a


newtype Vector
  = Vector [Double]

instance Nums Vector where
  add (Vector xs) (Vector ys)
    = Vector $ add' xs ys
      where
        add' :: [Double] -> [Double] -> [Double]
        add' [] ys
          = ys
        add' xs []
          = xs
        add' (x:xs) (y:ys)
          = (x + y) : add' xs ys

  scale scalar (Vector xs)
    = Vector $ map (* scalar) xs

  neg vector
    = scale (-1) vector

  sub vector1 vector2
    = add vector1 (neg vector2)

newtype Matrix
  = Matrix [Vector]
