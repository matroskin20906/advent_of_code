{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day4.Solution
  ( firstPart,
    secondPart,
  )
where

import Modules.Movements (Movement (Down, DownLeft, DownRight, Left, Right, Up, UpLeft, UpRight), toVector)
import Prelude hiding (Left, Right)

addStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
addStep coord1 coord2 = (fst coord1 + fst coord2, snd coord1 + snd coord2)

validCoord :: (Int, Int) -> Int -> Bool
validCoord coord len = fst coord >= 0 && snd coord >= 0 && fst coord < len && snd coord < len

analyseDirection :: [String] -> (Int, Int) -> Movement -> Int
analyseDirection [] _ _ = 0
analyseDirection matrix coor dir =
  let firstStep = addStep coor (toVector dir)
      secondStep = addStep firstStep (toVector dir)
      thirdStep = addStep secondStep (toVector dir)
   in if not (validCoord firstStep (length matrix)) || not (validCoord secondStep (length matrix)) || not (validCoord thirdStep (length matrix))
        then 0
        else
          if matrix !! snd firstStep !! fst firstStep == 'M' && matrix !! snd secondStep !! fst secondStep == 'A' && matrix !! snd thirdStep !! fst thirdStep == 'S'
            then 1
            else 0

analyseX :: [String] -> (Int, Int) -> Int
analyseX [] _ = 0
analyseX matrix coor =
  sum
    [ analyseDirection matrix coor Down,
      analyseDirection matrix coor Up,
      analyseDirection matrix coor Right,
      analyseDirection matrix coor Left,
      analyseDirection matrix coor UpRight,
      analyseDirection matrix coor UpLeft,
      analyseDirection matrix coor DownRight,
      analyseDirection matrix coor DownLeft
    ]

analyseStep2 :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
analyseStep2 matrix firstStep secondStep thirdStep fourthStep
  | not (validCoord firstStep (length matrix)) || not (validCoord secondStep (length matrix)) || not (validCoord thirdStep (length matrix)) || not (validCoord fourthStep (length matrix)) = 0
  | matrix !! snd firstStep !! fst firstStep == 'M'
      && matrix !! snd secondStep !! fst secondStep == 'M'
      && matrix !! snd thirdStep !! fst thirdStep == 'S'
      && matrix !! snd fourthStep !! fst fourthStep == 'S' =
      1
  | otherwise = 0

analyseDirection2 :: [String] -> (Int, Int) -> Movement -> Int
analyseDirection2 [] _ _ = 0
analyseDirection2 matrix coor UpRight =
  let firstStep = addStep coor (toVector UpRight)
      secondStep = addStep coor (toVector DownRight)
      thirdStep = addStep coor (toVector DownLeft)
      fourthStep = addStep coor (toVector UpLeft)
   in analyseStep2 matrix firstStep secondStep thirdStep fourthStep
analyseDirection2 matrix coor UpLeft =
  let firstStep = addStep coor (toVector UpLeft)
      secondStep = addStep coor (toVector UpRight)
      thirdStep = addStep coor (toVector DownRight)
      fourthStep = addStep coor (toVector DownLeft)
   in analyseStep2 matrix firstStep secondStep thirdStep fourthStep
analyseDirection2 matrix coor DownRight =
  let firstStep = addStep coor (toVector DownRight)
      secondStep = addStep coor (toVector DownLeft)
      thirdStep = addStep coor (toVector UpLeft)
      fourthStep = addStep coor (toVector UpRight)
   in analyseStep2 matrix firstStep secondStep thirdStep fourthStep
analyseDirection2 matrix coor DownLeft =
  let firstStep = addStep coor (toVector DownLeft)
      secondStep = addStep coor (toVector UpLeft)
      thirdStep = addStep coor (toVector UpRight)
      fourthStep = addStep coor (toVector DownRight)
   in analyseStep2 matrix firstStep secondStep thirdStep fourthStep

analyseXmas :: [String] -> (Int, Int) -> Int
analyseXmas [] _ = 0
analyseXmas matrix coor =
  sum
    [ analyseDirection2 matrix coor UpRight,
      analyseDirection2 matrix coor UpLeft,
      analyseDirection2 matrix coor DownRight,
      analyseDirection2 matrix coor DownLeft
    ]

analyseMatrix :: [String] -> (Int, Int) -> [Int]
analyseMatrix matrix coord
  | length matrix <= snd coord = [0]
  | length matrix <= snd coord && length matrix <= fst coord = [0]
  | length matrix <= fst coord =
      let nextCoord = (0, snd coord + 1)
       in analyseMatrix matrix nextCoord
  | otherwise =
      let nextCoord = addStep coord (toVector Right)
       in if matrix !! snd coord !! fst coord == 'X'
            then analyseX matrix coord : analyseMatrix matrix nextCoord
            else analyseMatrix matrix nextCoord

analyseMatrix2 :: [String] -> (Int, Int) -> [Int]
analyseMatrix2 matrix coord
  | length matrix <= snd coord = [0]
  | length matrix <= snd coord && length matrix <= fst coord = [0]
  | length matrix <= fst coord =
      let nextCoord = (0, snd coord + 1)
       in analyseMatrix2 matrix nextCoord
  | otherwise =
      let nextCoord = addStep coord (toVector Right)
       in if matrix !! snd coord !! fst coord == 'A'
            then analyseXmas matrix coord : analyseMatrix2 matrix nextCoord
            else analyseMatrix2 matrix nextCoord

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let matrix = lines contents
  print $ sum $ analyseMatrix matrix (0, 0)

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let matrix = lines contents
  print $ sum $ analyseMatrix2 matrix (0, 0)
