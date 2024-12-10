module Modules.Vectors
  ( Vector,
    sumVectors,
    toPair,
  )
where

data Vector = Vector Int Int

-- TODO: finish Vector module

sumVectors :: Vector -> Vector -> Vector
sumVectors (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

toPair :: Vector -> (Int, Int)
toPair (Vector x y) = (x, y)
