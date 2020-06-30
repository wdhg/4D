class Nums a where
  add :: a -> a -> a
  sub :: a -> a -> a
  scale :: Double -> a -> a
  neg :: a -> a


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

newtype Matrix
  = Matrix [Vector]

