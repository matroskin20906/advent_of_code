module Modules.Movements
  ( toVector,
    turnRight,
    Movement (..),
  )
where

import Prelude hiding (Left, Right)

data Movement = Up | Down | Right | Left | UpRight | UpLeft | DownRight | DownLeft

turnRight :: Movement -> Movement
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

instance Eq Movement where
  m1 == m2 = fst (toVector m1) == fst (toVector m2) && snd (toVector m1) == snd (toVector m2)

instance Ord Movement where
  compare m1 m2
    | fst (toVector m1) < fst (toVector m2) = LT
    | fst (toVector m1) > fst (toVector m2) = GT
    | snd (toVector m1) < snd (toVector m2) = LT
    | snd (toVector m1) > snd (toVector m2) = GT
    | otherwise = EQ

toVector :: Movement -> (Int, Int)
toVector Up = (0, -1)
toVector Down = (0, 1)
toVector Right = (1, 0)
toVector Left = (-1, 0)
toVector UpRight = (1, -1)
toVector UpLeft = (-1, -1)
toVector DownRight = (1, 1)
toVector DownLeft = (-1, 1)
