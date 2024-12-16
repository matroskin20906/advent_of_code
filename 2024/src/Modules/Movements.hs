module Modules.Movements
  ( toVector,
    turnRight,
    turnLeft,
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

turnLeft :: Movement -> Movement
turnLeft Up = Left
turnLeft Right = Up
turnLeft Left = Down
turnLeft Down = Right

instance Eq Movement where
  m1 == m2 = fst (toVector m1) == fst (toVector m2) && snd (toVector m1) == snd (toVector m2)

instance Show Movement where
  show m = toString m

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

toString :: Movement -> String
toString Up = "Up"
toString Down = "Down"
toString Left = "Left"
toString Right = "Right"
