module Day6.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.Bifunctor qualified
import Data.Set (Set, delete, empty, findMin, fromList, insert, member, size)
import Modules.Movements (Movement (Up), toVector, turnRight)
import Prelude hiding (Left, Right)

sumIntPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumIntPair p1 = Data.Bifunctor.bimap (fst p1 +) (snd p1 +)

isValid :: [[a]] -> (Int, Int) -> Bool
isValid matrix coord
  | fst coord < 0 || fst coord >= length matrix || snd coord < 0 || snd coord >= length matrix = False
  | otherwise = True

value :: [[Char]] -> (Int, Int) -> Char
value matrix coord = matrix !! snd coord !! fst coord

makeStep :: [[Char]] -> (Int, Int) -> Movement -> ((Int, Int), Movement)
makeStep matrix coords dir =
  let newCoord = sumIntPair coords (toVector dir)
   in if not (isValid matrix newCoord)
        then ((-1, -1), dir)
        else
          if value matrix newCoord /= '#'
            then (newCoord, dir)
            else makeStep matrix coords (turnRight dir)

makeSteps :: [[Char]] -> (Int, Int) -> Movement -> [(Int, Int)]
makeSteps matrix coords dir =
  if fst coords == -1 && snd coords == -1
    then []
    else
      let s = makeStep matrix coords dir
       in coords : uncurry (makeSteps matrix) s

startPos :: [[Char]] -> (Int, Int)
startPos m =
  helper m (0, 0)
  where
    helper :: [[Char]] -> (Int, Int) -> (Int, Int)
    helper matrix coord
      | value matrix coord == '^' = coord
      | fst coord + 1 >= length matrix = helper matrix (0, snd coord + 1)
      | otherwise = helper matrix (fst coord + 1, snd coord)

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let matrix = lines contents
  print $ length $ fromList $ makeSteps matrix (startPos matrix) Up

updateList :: Int -> a -> [a] -> [a]
updateList idx newVal xs = take idx xs ++ [newVal] ++ drop (idx + 1) xs

updateMatrix :: Int -> Int -> a -> [[a]] -> [[a]]
updateMatrix row col newVal matrix =
  updateList row updatedRow matrix
  where
    updatedRow = updateList col newVal (matrix !! row)

testMatrix :: [[Char]] -> Int
testMatrix matrix =
  helper matrix (startPos matrix) Up empty
  where
    helper :: [[Char]] -> (Int, Int) -> Movement -> Set ((Int, Int), Movement) -> Int
    helper m p d v =
      let nextStep = makeStep m p d
       in if fst nextStep == (-1, -1)
            then 0
            else if member nextStep v then 1 else helper m (fst nextStep) (snd nextStep) (insert nextStep v)

testNewMatrixs2 :: [[Char]] -> Set (Int, Int) -> Int
testNewMatrixs2 matrix steps =
  let s = findMin steps
   in if size steps == 0
        then 0
        else
          if value matrix s == '.'
            then testMatrix (updateMatrix (snd s) (fst s) '#' matrix) + testNewMatrixs2 matrix (delete s steps)
            else testNewMatrixs2 matrix (delete s steps)

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let matrix = lines contents
  print $ testNewMatrixs2 matrix (fromList $ makeSteps matrix (startPos matrix) Up)
