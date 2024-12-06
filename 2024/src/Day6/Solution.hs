module Day6.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.Bifunctor qualified
import Data.Set (Set, empty, fromList, insert, member)
import Prelude hiding (Left, Right)

data Movement = Up | Down | Left | Right

instance Eq Movement where
  m1 == m2 = fst (step m1) == fst (step m2) && snd (step m1) == snd (step m2)

instance Ord Movement where
  compare m1 m2
    | fst (step m1) < fst (step m2) = LT
    | fst (step m1) > fst (step m2) = GT
    | snd (step m1) < snd (step m2) = LT
    | snd (step m1) > snd (step m2) = GT
    | otherwise = EQ

step :: Movement -> (Int, Int)
step Up = (0, -1)
step Down = (0, 1)
step Left = (-1, 0)
step Right = (1, 0)

turnRight :: Movement -> Movement
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

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
  let newCoord = sumIntPair coords (step dir)
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

nextCoord matrix coord =
  let newCoord = sumIntPair coord (step Right)
   in if isValid matrix newCoord
        then newCoord
        else if snd coord + 1 < length matrix then (0, snd coord + 1) else (-1, -1)

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

testNewMatrixs :: [[Char]] -> (Int, Int) -> Int
testNewMatrixs matrix coord =
  let newCoord = nextCoord matrix coord
   in if newCoord == (-1, -1)
        then 0
        else
          if value matrix coord == '.'
            then
              testMatrix (updateMatrix (snd coord) (fst coord) '#' matrix)
                + testNewMatrixs matrix newCoord
            else testNewMatrixs matrix newCoord

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let matrix = lines contents
  print $ testNewMatrixs matrix (0, 0)
