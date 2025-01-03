module Day12.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.List (sortOn)
import Modules.Movements (Movement (Down, Left, Right, Up), toVector)
import Prelude hiding (Left, Right)

step :: (Int, Int) -> (Int, Int) -> (Int, Int)
step (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getRegionsByName :: Char -> [(Char, [(Int, Int)])] -> [(Char, [(Int, Int)])]
getRegionsByName name = filter (\el -> fst el == name)

isInsideExistingRegion :: (Int, Int) -> [(Char, [(Int, Int)])] -> Bool
isInsideExistingRegion _ [] = False
isInsideExistingRegion coord (region : regions) =
  elem coord (snd region) || isInsideExistingRegion coord regions

notInsideMatrix :: (Int, Int) -> Int -> Bool
notInsideMatrix (x, y) matrixLength = x < 0 || x >= matrixLength || y < 0 || y >= matrixLength

value :: [[a]] -> (Int, Int) -> a
value matrix (x, y) = matrix !! y !! x

calcRegionPrice :: (Char, [(Int, Int)]) -> Int
calcRegionPrice region = calcRegionArea region * calcRegionPerimeter region

calcRegionSides :: (Char, [(Int, Int)]) -> [((Int, Int), (Int, Int))]
calcRegionSides region =
  helper (snd region) (snd region)
  where
    helper :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
    helper [] _ = []
    helper (coord : tl) r =
      regionBorders coord r ++ helper tl r

countLines :: [((Int, Int), (Int, Int))] -> Int
countLines linesParts =
  let linesX = makeLinesX linesParts
      linesY = makeLinesY linesParts
   in 2 * length (filter (\el -> length el /= 0) (getAllCrosses linesX linesY)) + length linesX + length linesY

getAllCrosses :: (Ord a1, Ord a2, Ord a3, Ord a4) => [((a1, a2), (a3, a4))] -> [((a1, a2), (a3, a4))] -> [(((a1, a2), (a3, a4)), ((a1, a2), (a3, a4)))]
getAllCrosses [] _ = []
getAllCrosses (lineX : linesX) linesY =
  let crosses = getCrosses lineX linesY
   in if crosses == []
        then getAllCrosses linesX linesY
        else crosses ++ getAllCrosses linesX linesY

getCrosses :: (Ord a1, Ord a2, Ord a3, Ord a4) => ((a1, a2), (a3, a4)) -> [((a1, a2), (a3, a4))] -> [(((a1, a2), (a3, a4)), ((a1, a2), (a3, a4)))]
getCrosses _ [] = []
getCrosses lineX (lineY : linesY) =
  if isCross lineX lineY then (lineX, lineY) : getCrosses lineX linesY else getCrosses lineX linesY

isCross :: (Ord a1, Ord a2, Ord a3, Ord a4) => ((a1, a2), (a3, a4)) -> ((a1, a2), (a3, a4)) -> Bool
isCross ((xx1, xx2), (xy1, xy2)) ((yx1, yx2), (yy1, yy2)) =
  xx1 < yx1 && xx2 > yx2 && yy1 < xy1 && yy2 > xy2

makeLinesX :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
makeLinesX [] = []
makeLinesX (part : tl) =
  filter
    (\el -> abs (fst (snd el) - snd (snd el)) == 1)
    ( map (\el -> (el, snd part)) (smushLines (map fst (sortOn (\el -> fst (fst el)) (part : filter (\el -> snd el == snd part) tl))))
        ++ makeLinesX (filter (\el -> snd el /= snd part) (part : tl))
    )

makeLinesY :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
makeLinesY [] = []
makeLinesY (part : tl) =
  filter
    (\el -> abs (fst (fst el) - snd (fst el)) == 1)
    ( map (\el -> (fst part, el)) (smushLines (map snd (sortOn (\el -> fst (snd el)) (part : filter (\el -> fst el == fst part) tl))))
        ++ makeLinesY (filter (\el -> fst el /= fst part) (part : tl))
    )

smushLines :: (Eq b, Num b) => [(b, b)] -> [(b, b)]
smushLines [] = []
smushLines [lmnt] = [lmnt]
smushLines ((c1, c11) : (c2, c22) : tl) =
  if c11 - 1 == c2 then smushLines ((c1, c22) : tl) else (c1, c11) : smushLines ((c2, c22) : tl)

regionBorders :: (Int, Int) -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
regionBorders coord l =
  let up = step coord (toVector Up)
      down = step coord (toVector Down)
      left = step coord (toVector Left)
      right = step coord (toVector Right)
      x = fst coord
      y = snd coord
   in map fst (filter (not . snd) [(((x - 1, x + 1), (y - 1, y)), up `elem` l), (((x - 1, x + 1), (y, y + 1)), down `elem` l), (((x - 1, x), (y - 1, y + 1)), left `elem` l), (((x, x + 1), (y - 1, y + 1)), right `elem` l)])

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
  | el `elem` l = 1
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
       in if notInsideMatrix coord matrixLength || value matrix coord /= name || elem coord visited
            then visited
            else
              helper
                matrix
                matrixLength
                right
                name
                ( helper
                    matrix
                    matrixLength
                    down
                    name
                    ( helper
                        matrix
                        matrixLength
                        left
                        name
                        ( helper matrix matrixLength up name (coord : visited)
                        )
                    )
                )

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

calcRegionPrice2 :: [Int] -> [Int] -> Int
calcRegionPrice2 [] _ = 0
calcRegionPrice2 _ [] = 0
calcRegionPrice2 (area : areas) (side : sides) = area * side + calcRegionPrice2 areas sides

secondPart :: FilePath -> IO ()
secondPart file = do
  contents <- readFile file
  let matrix = lines contents
  let regions = mapRegions matrix
  let areas = map calcRegionArea regions
  print $ calcRegionPrice2 areas (map countLines (map calcRegionSides regions))
