module Day12.Solution
  ( firstPart,
    secondPart,
  )
where

import Modules.Movements (Movement (Down, Left, Right, Up), toVector)
import Prelude hiding (Left, Right)

step :: (Int, Int) -> (Int, Int) -> (Int, Int)
step (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getRegionsByName :: Char -> [(Char, [(Int, Int)])] -> [(Char, [(Int, Int)])]
getRegionsByName name regions = filter (\el -> fst el == name) regions

isInsideExistingRegion :: (Int, Int) -> [(Char, [(Int, Int)])] -> Bool
isInsideExistingRegion _ [] = False
isInsideExistingRegion coord (region : regions) =
  (elem coord (snd region)) || isInsideExistingRegion coord regions

notInsideMatrix :: (Int, Int) -> Int -> Bool
notInsideMatrix (x, y) matrixLength = x < 0 || x >= matrixLength || y < 0 || y >= matrixLength

insideMatrix :: (Int, Int) -> Int -> Bool
insideMatrix (x, y) matrixLength = x >= 0 && x < matrixLength && y >= 0 && y < matrixLength

value :: [[a]] -> (Int, Int) -> a
value matrix (x, y) = matrix !! y !! x

calcRegionPrice :: (Char, [(Int, Int)]) -> Int
calcRegionPrice region = calcRegionArea region * calcRegionPerimeter region

calcRegionArea :: (Char, [(Int, Int)]) -> Int
calcRegionArea region = length (snd region)

calcRegionPerimeter :: (Char, [(Int, Int)]) -> Int
calcRegionPerimeter region =
  helper (snd region) (snd region)
  where
    helper :: [(Int, Int)] -> [(Int, Int)] -> Int
    helper [] _ = 0
    helper (c : tl) cs = 4 - neightbourCount c cs + helper tl cs

neightbourCount :: (Int, Int) -> [(Int, Int)] -> Int
neightbourCount _ [] = 0
neightbourCount coord l =
  let up = step coord (toVector Up)
      down = step coord (toVector Down)
      left = step coord (toVector Left)
      right = step coord (toVector Right)
   in myElem up l + myElem down l + myElem left l + myElem right l

myElem :: (Eq a) => a -> [a] -> Int
myElem el l
  | elem el l = 1
  | otherwise = 0

findRegion :: [[Char]] -> (Int, Int) -> Char -> (Char, [(Int, Int)])
findRegion m c n =
  (n, helper m (length m) c n [])
  where
    helper :: [[Char]] -> Int -> (Int, Int) -> Char -> [(Int, Int)] -> [(Int, Int)]
    helper matrix matrixLength coord name visited =
      let up = step coord (toVector Up)
          down = step coord (toVector Down)
          left = step coord (toVector Left)
          right = step coord (toVector Right)
       in if notInsideMatrix coord matrixLength
            then coord : visited
            else
              if insideMatrix right matrixLength && value matrix right == name && not (elem right visited)
                then helper matrix matrixLength right name (coord : visited)
                else
                  if insideMatrix down matrixLength && value matrix down == name && not (elem down visited)
                    then helper matrix matrixLength down name (coord : visited)
                    else
                      if insideMatrix left matrixLength && value matrix left == name && not (elem left visited)
                        then helper matrix matrixLength left name (coord : visited)
                        else
                          if insideMatrix up matrixLength && value matrix up == name && not (elem up visited)
                            then helper matrix matrixLength up name (coord : visited)
                            else coord : visited

mapRegions :: [[Char]] -> [(Char, [(Int, Int)])]
mapRegions [] = []
mapRegions m =
  helper m (0, 0) (length m) []
  where
    helper :: [[Char]] -> (Int, Int) -> Int -> [(Char, [(Int, Int)])] -> [(Char, [(Int, Int)])]
    helper matrix coord ml res
      | fst coord >= ml = helper matrix (0, snd coord + 1) ml res
      | snd coord >= ml = res
      | otherwise =
          if isInsideExistingRegion coord (getRegionsByName (value matrix coord) res)
            then helper matrix (fst coord + 1, snd coord) ml res
            else helper matrix (fst coord + 1, snd coord) ml (findRegion matrix coord (value matrix coord) : res)

firstPart :: FilePath -> IO ()
firstPart file = do
  contents <- readFile file
  let matrix = lines contents
  print $ sum $ map calcRegionPrice (mapRegions matrix)

secondPart :: FilePath -> IO ()
secondPart file = do
  contents <- readFile file
  let matrix = lines contents
  print matrix
