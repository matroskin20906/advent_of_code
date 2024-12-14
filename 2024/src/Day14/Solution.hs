module Day14.Solution
  ( firstPart,
    secondPart,
  )
where

import Text.Regex.Posix ((=~))

matches :: (a, b, c, d) -> d
matches (_, _, _, l) = l

robotToInts :: String -> ((Int, Int), (Int, Int))
robotToInts [] = ((-1, -1), (-1, -1))
robotToInts s =
  let regex = "p=([^,]*),([^ ]*) v=([^,]*),([^ ]*)"
      robot = matches (s =~ regex :: (String, String, String, [String]))
   in if length robot > 1
        then ((read (head robot), read (robot !! 1)), (read (robot !! 2), read (last robot)))
        else ((-1, -1), (-1, -1))

sumVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

teleportXY :: Int -> Int -> Int
teleportXY ms c
  | c < 0 = ms + c
  | c >= ms = c - ms
  | otherwise = c

teleport :: Int -> Int -> (Int, Int) -> (Int, Int)
teleport mh ml (x, y) = (teleportXY ml x, teleportXY mh y)

robotStep :: Int -> Int -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
robotStep mh ml ((px, py), (vx, vy)) =
  (teleport mh ml (sumVectors (px, py) (vx, vy)), (vx, vy))

robotsStep :: Int -> Int -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
robotsStep _ _ [] = []
robotsStep mh ml (robot : tl) = robotStep mh ml robot : robotsStep mh ml tl

timer :: Int -> Int -> Int -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
timer _ _ 0 robots = robots
timer mh ml t robots = timer mh ml (t - 1) (robotsStep mh ml robots)

toStrings [] = ""
toStrings ((x, y) : tl) = show x ++ "," ++ show y ++ "\n" ++ toStrings tl

timer2 :: (Ord t, Num t) => Int -> Int -> t -> [((Int, Int), (Int, Int))] -> [(t, [(Int, Int)])]
timer2 matrixHight matrixLength times r =
  helper matrixHight matrixLength times 1 r
  where
    helper mh ml t stepNow robots =
      let step = robotsStep mh ml robots
       in if stepNow < t
            then (stepNow, map fst step) : helper mh ml t (stepNow + 1) step
            else []

-- writeFile ("day14files2/" ++ show stepNow ++ ".txt") (toStrings step)
writeAnomalies [] = return ()
writeAnomalies ((n, step) : tl) = do
  writeFile ("day14files2/" ++ show n ++ ".txt") (toStrings step)
  writeAnomalies tl

countInQuadrant :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int
countInQuadrant _ _ [] = 0
countInQuadrant (nex, ney) (swx, swy) ((x, y) : tl) =
  if x >= nex && x <= swx && y >= ney && y <= swy
    then 1 + countInQuadrant (nex, ney) (swx, swy) tl
    else countInQuadrant (nex, ney) (swx, swy) tl

firstPart :: FilePath -> IO ()
firstPart file = do
  contents <- readFile file
  let ls = lines contents
  let matrixHight = 103
  let matrixLength = 101
  let endCoords = map fst $ timer matrixHight matrixLength 100 (map robotToInts ls)
  print
    ( countInQuadrant (0, 0) (div matrixLength 2 - 1, div matrixHight 2 - 1) endCoords
        * countInQuadrant (div matrixLength 2 + 1, 0) (matrixLength - 1, div matrixHight 2 - 1) endCoords
        * countInQuadrant (0, div matrixHight 2 + 1) (div matrixLength 2 - 1, matrixHight - 1) endCoords
        * countInQuadrant (div matrixLength 2 + 1, div matrixHight 2 + 1) (matrixLength - 1, matrixLength - 1) endCoords
    )

secondPart :: FilePath -> IO ()
secondPart file = do
  contents <- readFile file
  let ls = lines contents
  let matrixHight = 103
  let matrixLength = 101
  writeAnomalies $ timer2 matrixHight matrixLength 50000 (map robotToInts ls)
  print ls
