{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day4.Solution
  ( firstPart,
    secondPart,
  )
where

data Direction = Down | Up | Forward | Backward | UpRight | UpLeft | DownRight | DownLeft
  deriving (Eq, Show)

getDirection :: Direction -> (Int, Int)
getDirection Down = (0, 1)
getDirection Up = (0, -1)
getDirection Forward = (1, 0)
getDirection Backward = (-1, 0)
getDirection UpRight = (1, -1)
getDirection UpLeft = (-1, -1)
getDirection DownRight = (1, 1)
getDirection DownLeft = (-1, 1)

addStep :: (Int, Int) -> (Int, Int) -> (Int, Int)
addStep coord1 coord2 = (fst coord1 + fst coord2, snd coord1 + snd coord2)

validCoord :: (Int, Int) -> Int -> Bool
validCoord coord len = fst coord >= 0 && snd coord >= 0 && fst coord < len && snd coord < len

analyseDirection :: [String] -> (Int, Int) -> Direction -> Int
analyseDirection [] _ _ = 0
analyseDirection matrix coor dir =
  let firstStep = addStep coor (getDirection dir)
      secondStep = addStep firstStep (getDirection dir)
      thirdStep = addStep secondStep (getDirection dir)
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
      analyseDirection matrix coor Forward,
      analyseDirection matrix coor Backward,
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

analyseDirection2 :: [String] -> (Int, Int) -> Direction -> Int
analyseDirection2 [] _ _ = 0
analyseDirection2 matrix coor UpRight =
  let firstStep = addStep coor (getDirection UpRight)
      secondStep = addStep coor (getDirection DownRight)
      thirdStep = addStep coor (getDirection DownLeft)
      fourthStep = addStep coor (getDirection UpLeft)
   in analyseStep2 matrix firstStep secondStep thirdStep fourthStep
analyseDirection2 matrix coor UpLeft =
  let firstStep = addStep coor (getDirection UpLeft)
      secondStep = addStep coor (getDirection UpRight)
      thirdStep = addStep coor (getDirection DownRight)
      fourthStep = addStep coor (getDirection DownLeft)
   in analyseStep2 matrix firstStep secondStep thirdStep fourthStep
analyseDirection2 matrix coor DownRight =
  let firstStep = addStep coor (getDirection DownRight)
      secondStep = addStep coor (getDirection DownLeft)
      thirdStep = addStep coor (getDirection UpLeft)
      fourthStep = addStep coor (getDirection UpRight)
   in analyseStep2 matrix firstStep secondStep thirdStep fourthStep
analyseDirection2 matrix coor DownLeft =
  let firstStep = addStep coor (getDirection DownLeft)
      secondStep = addStep coor (getDirection UpLeft)
      thirdStep = addStep coor (getDirection UpRight)
      fourthStep = addStep coor (getDirection DownRight)
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
      let nextCoord = addStep coord (getDirection Forward)
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
      let nextCoord = addStep coord (getDirection Forward)
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
