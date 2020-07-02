module Math where

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

instance Eq Vector where
  vector1 == vector2
    = all ((< threshold) . abs) xs
      where
        threshold
          = 0.0000001
        Vector xs
          = vector1 `sub` vector2

instance Show Vector where
  show (Vector xs)
    = show xs

newtype Matrix
  = Matrix (Int, Int) [Vector]

instance Nums Matrix where
  add (Matrix (m1, n1) xs) (Matrix (m2, n2) ys)
    | m1 != m2 || n1 != n2  = error "cannot add different dimension matrices"
    | otherwise             = Matrix $ zipWith add xs ys
