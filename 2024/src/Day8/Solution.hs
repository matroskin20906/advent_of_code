module Day8.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.Set (Set, empty, fromList, size, union)

isAntenna :: Char -> Bool
isAntenna '.' = False
isAntenna _ = True

findAllAntennas :: [[Char]] -> [(Char, (Int, Int))]
findAllAntennas m =
  helper m (0, 0)
  where
    helper :: [[Char]] -> (Int, Int) -> [(Char, (Int, Int))]
    helper matrix (x, y)
      | y >= length matrix = []
      | x >= length matrix = helper matrix (0, y + 1)
      | isAntenna (matrix !! y !! x) = (matrix !! y !! x, (x, y)) : helper matrix (x + 1, y)
      | otherwise = helper matrix (x + 1, y)

isInMatrix :: Int -> (Int, Int) -> Bool
isInMatrix matrixSize (x, y)
  | x < 0 || x >= matrixSize = False
  | y < 0 || y >= matrixSize = False
  | otherwise = True

findAntennaPairs :: (Char, (Int, Int)) -> [(Char, (Int, Int))] -> [(Char, (Int, Int))]
findAntennaPairs antenna antennas =
  let sameAntennas = filter (\el -> fst antenna == fst el) antennas
   in helper antenna sameAntennas
  where
    helper :: (Char, (Int, Int)) -> [(Char, (Int, Int))] -> [(Char, (Int, Int))]
    helper _ [] = []
    helper a (sa : sas)
      | a == sa = helper a sas
      | fst a == fst sa = sa : helper a sas
      | otherwise = helper a sas

countNeededAntinodes :: [(Char, [(Int, Int)])] -> Int -> Int
countNeededAntinodes antennasGroups matrixSize =
  helper antennasGroups matrixSize empty
  where
    helper :: [(Char, [(Int, Int)])] -> Int -> Set (Int, Int) -> Int
    helper [] _ s = size s
    helper (group : tl) ms s = helper tl ms (s `union` fromList (processOneGroup (fst group) (snd group) ms))

processOneGroup :: Char -> [(Int, Int)] -> Int -> [(Int, Int)]
processOneGroup n coords matrixSize =
  helper n coords coords matrixSize
  where
    helper _ [] _ _ = []
    helper antenna (coord : coords1) coords2 ms = processOneGroupHelper antenna coord coords2 ms ++ helper antenna coords1 coords2 ms

processOneGroupHelper :: Char -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
processOneGroupHelper _ _ [] _ = []
processOneGroupHelper n coord (coord2 : group) matrixSize =
  if coord == coord2
    then processOneGroupHelper n coord group matrixSize
    else
      let antinodes = findAntinodesCoords coord coord2
          isFirstInMatrix = isInMatrix matrixSize (fst antinodes)
          isSecondInMatrix = isInMatrix matrixSize (snd antinodes)
       in if isFirstInMatrix && isSecondInMatrix
            then fst antinodes : snd antinodes : processOneGroupHelper n coord group matrixSize
            else
              if isFirstInMatrix
                then fst antinodes : processOneGroupHelper n coord group matrixSize
                else
                  if isSecondInMatrix
                    then snd antinodes : processOneGroupHelper n coord group matrixSize
                    else processOneGroupHelper n coord group matrixSize

findWithMinY :: (Ord b) => (a, b) -> (a, b) -> (a, b)
findWithMinY (x1, y1) (x2, y2)
  | y1 < y2 = (x1, y1)
  | otherwise = (x2, y2)

findWithMaxY :: (Ord b) => (a, b) -> (a, b) -> (a, b)
findWithMaxY (x1, y1) (x2, y2)
  | y1 > y2 = (x1, y1)
  | otherwise = (x2, y2)

findAntinodesCoords :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
findAntinodesCoords (x1, y1) (x2, y2) =
  let withMinY = findWithMinY (x1, y1) (x2, y2)
      withMaxY = findWithMaxY (x1, y1) (x2, y2)
   in if fst withMinY > fst withMaxY
        then ((fst withMinY + abs (x1 - x2), snd withMinY - abs (y1 - y2)), (fst withMinY - (2 * abs (x1 - x2)), snd withMinY + (2 * abs (y1 - y2))))
        else ((fst withMinY - abs (x1 - x2), snd withMinY - abs (y1 - y2)), (fst withMinY + (2 * abs (x1 - x2)), snd withMinY + (2 * abs (y1 - y2))))

makeAntennasGroups :: [(Char, (Int, Int))] -> [(Char, [(Int, Int)])]
makeAntennasGroups [] = []
makeAntennasGroups (antenna : antennas) =
  let pairs = findAntennaPairs antenna antennas
   in (fst antenna, snd antenna : map snd pairs) : makeAntennasGroups (filter (\el -> fst antenna /= fst el) antennas)

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let ls = lines contents
  print $ countNeededAntinodes (makeAntennasGroups $ findAllAntennas ls) (length ls)

countNeededAntinodes2 :: [(Char, [(Int, Int)])] -> Int -> Int
countNeededAntinodes2 antennasGroups matrixSize =
  helper antennasGroups matrixSize empty
  where
    helper :: [(Char, [(Int, Int)])] -> Int -> Set (Int, Int) -> Int
    helper [] _ s = size s
    helper (group : tl) ms s = helper tl ms (s `union` fromList (processOneGroup2 (fst group) (snd group) ms))

processOneGroup2 :: Char -> [(Int, Int)] -> Int -> [(Int, Int)]
processOneGroup2 n coords matrixSize =
  helper n coords coords matrixSize
  where
    helper _ [] _ _ = []
    helper antenna (coord : coords1) coords2 ms = processOneGroupHelper2 antenna coord coords2 ms ++ helper antenna coords1 coords2 ms

processOneGroupHelper2 :: Char -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
processOneGroupHelper2 _ _ [] _ = []
processOneGroupHelper2 n coord (coord2 : group) matrixSize =
  if coord == coord2
    then processOneGroupHelper2 n coord group matrixSize
    else
      let antinodes = findAntinodesCoords2 coord coord2 matrixSize
       in filter (isInMatrix matrixSize) antinodes ++ processOneGroupHelper2 n coord group matrixSize

findAntinodesCoords2 :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
findAntinodesCoords2 (x1, y1) (x2, y2) matrixSize =
  let withMinY = findWithMinY (x1, y1) (x2, y2)
      withMaxY = findWithMaxY (x1, y1) (x2, y2)
   in if fst withMinY > fst withMaxY
        then (withMinY : calcPlus withMinY (abs (x1 - x2), -1 * (abs (y1 - y2))) matrixSize) ++ calcMinus withMinY (abs (x1 - x2), -1 * abs (y1 - y2)) matrixSize
        else (withMinY : calcPlus withMinY (abs (x1 - x2), abs (y1 - y2)) matrixSize) ++ calcMinus withMinY ((abs (x1 - x2), abs (y1 - y2))) matrixSize

calcPlus :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
calcPlus (x, y) (dx, dy) matrixSize =
  let newCoord = (x + dx, y + dy)
   in if isInMatrix matrixSize newCoord
        then newCoord : calcPlus newCoord (dx, dy) matrixSize
        else []

calcMinus :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
calcMinus (x, y) (dx, dy) matrixSize =
  let newCoord = (x - dx, y - dy)
   in if isInMatrix matrixSize newCoord
        then newCoord : calcMinus newCoord (dx, dy) matrixSize
        else []

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let ls = lines contents
  print $ countNeededAntinodes2 (makeAntennasGroups $ findAllAntennas ls) (length ls)
