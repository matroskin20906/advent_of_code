module Day10.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.Char (digitToInt)
import Data.Set (fromList)
import Modules.Movements (Movement (Down, Left, Right, Up), toVector)
import Prelude hiding (Left, Right)

sumVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

isInMatrix :: Int -> (Int, Int) -> Bool
isInMatrix s (x, y)
  | x < 0 || x >= s = False
  | y < 0 || y >= s = False
  | otherwise = True

findAll9 :: [[Char]] -> [(Int, (Int, Int))]
findAll9 m =
  helper m (0, 0)
  where
    helper matrix (x, y)
      | y >= length matrix = []
      | x >= length matrix = helper matrix (0, y + 1)
      | otherwise =
          if matrix !! y !! x == '9'
            then (9, (x, y)) : helper matrix (x + 1, y)
            else helper matrix (x + 1, y)

process9 :: [[Char]] -> Int -> (Int, (Int, Int)) -> [(Int, Int)] -> [[(Int, Int)]]
process9 m ms t p =
  let up = sumVectors (snd t) (toVector Up)
      down = sumVectors (snd t) (toVector Down)
      left = sumVectors (snd t) (toVector Left)
      right = sumVectors (snd t) (toVector Right)
      x = fst (snd t)
      y = snd (snd t)
      digit = fst t
   in ( if not (isInMatrix ms (snd t)) || (digit /= digitToInt (m !! y !! x))
          then []
          else
            ( if fst t == 0
                then [reverse (snd t : p)]
                else
                  process9 m ms (digit - 1, up) (snd t : p)
                    ++ process9 m ms (digit - 1, down) (snd t : p)
                    ++ process9 m ms (digit - 1, left) (snd t : p)
                    ++ process9 m ms (digit - 1, right) (snd t : p)
            )
      )

process9s :: [[Char]] -> Int -> [(Int, (Int, Int))] -> [[(Int, Int)]]
process9s _ _ [] = []
process9s m ms (el : tl) =
  process9 m ms el [] ++ process9s m ms tl

headTl :: [a] -> [a]
headTl [] = []
headTl l = [head l, last l]

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let ls = lines contents
  let matrixSize = length ls
  let lists = process9s ls matrixSize (findAll9 ls)
  print $ length $ fromList $ map headTl (filter (\el -> length el == 10) lists)

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let ls = lines contents
  let matrixSize = length ls
  let lists = process9s ls matrixSize (findAll9 ls)
  print $ length $ fromList $ filter (\el -> length el == 10) lists
