module Day11.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.IntMap (IntMap, assocs, fromListWith)

blink :: Int -> [Int]
blink 0 = [1]
blink n
  | (w, 0) <- length (show n) `quotRem` 2,
    (l, r) <- n `quotRem` (10 ^ w) =
      [l, r]
blink n = [n * 2024]

blinks :: IntMap Int -> IntMap Int
blinks stones = fromListWith (+) [(stone', n) | (stone, n) <- assocs stones, stone' <- blink stone]

times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0 = x
  | otherwise = times (n - 1) f $! f x

steps :: Int -> [Int] -> Int
steps n input =
  sum
    ( times
        n
        blinks
        ( fromListWith
            (+)
            [ (i, 1)
              | i <- input
            ]
        )
    )

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let w = map (read :: String -> Int) (words contents)
  print $ steps 25 w

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let w = map (read :: String -> Int) (words contents)
  print $ steps 75 w
