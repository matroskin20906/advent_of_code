module Day16.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.Heap (MinHeap)
import Data.Heap qualified as Heap
import Data.Map (Map, findWithDefault)
import Data.Map qualified as Map
import Data.Set (Set, empty, insert, member)
import Data.Set qualified as Set
import Modules.Movements (Movement (Down, Left, Right, Up), toVector, turnLeft, turnRight)
import Prelude hiding (Left, Right)

sumVectors :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

value :: [[a]] -> (Int, Int) -> a
value matrix (x, y) = matrix !! y !! x

matrixToMap :: [[a]] -> Map (Int, Int) a
matrixToMap matrix =
  Map.fromList (helper matrix (length matrix) (0, 0))
  where
    helper m ml pos
      | snd pos >= ml = []
      | fst pos >= ml = helper m ml (0, snd pos + 1)
      | otherwise = (pos, value matrix pos) : helper m ml (sumVectors pos (1, 0))

findStartPos :: [[Char]] -> (Int, Int)
findStartPos matrix =
  helper matrix (length matrix) (0, 0)
  where
    helper m ml pos
      | snd pos >= ml = (-1, -1)
      | fst pos >= ml = helper m ml (0, snd pos + 1)
      | isStart (value matrix pos) = pos
      | otherwise = helper m ml (sumVectors pos (1, 0))

isStart :: Char -> Bool
isStart c = c == 'S'

isEnd :: Char -> Bool
isEnd c = c == 'E'

findWays :: Map (Int, Int) Char -> (Int, Int) -> Maybe (Set (Int, Int), Int)
findWays m p = dijkstra m (Heap.singleton ((0, p), Right)) Map.empty Set.empty

dijkstra :: Map (Int, Int) Char -> MinHeap ((Int, (Int, Int)), Movement) -> Map (Int, Int) Int -> Set (Int, Int) -> Maybe (Set (Int, Int), Int)
dijkstra matrix queue dist visited =
  case Heap.view queue of
    Nothing -> Nothing
    Just (((d, u), v), queue') ->
      let char = findWithDefault '#' u matrix
       in if isEnd char
            then Just (visited, d)
            else
              if char == '#' || member u visited
                then dijkstra matrix queue' dist visited
                else
                  let newVisited = insert u visited
                      neighbors = [(sumVectors (toVector mov) u, mov) | mov <- [Right, Left, Up, Down]]
                      (queue'', dist') = foldl (relax u d v) (queue', dist) neighbors
                   in dijkstra matrix queue'' dist' newVisited

relax :: (Int, Int) -> Int -> Movement -> (MinHeap ((Int, (Int, Int)), Movement), Map (Int, Int) Int) -> ((Int, Int), Movement) -> (MinHeap ((Int, (Int, Int)), Movement), Map (Int, Int) Int)
relax u d from (queue, dist) (v, mov)
  | alt < Map.findWithDefault maxBound v dist =
      (Heap.insert ((alt, v), mov) queue, Map.insert v alt dist)
  | otherwise = (queue, dist)
  where
    alt = d + movementCost from mov

movementCost :: Movement -> Movement -> Int
movementCost Right Right = 1
movementCost Right Up = 1001
movementCost Right Left = 1
movementCost Right Down = 1001
movementCost Left Up = 1001
movementCost Left Down = 1001
movementCost Left Left = 1
movementCost Left Right = 1
movementCost Up Up = 1
movementCost Up Down = 1
movementCost Up Left = 1001
movementCost Up Right = 1001
movementCost Down Down = 1
movementCost Down Up = 1
movementCost Down Left = 1001
movementCost Down Right = 1001

firstPart :: FilePath -> IO ()
firstPart file = do
  contents <- readFile file
  let matrix = lines contents
  print $ findWays (matrixToMap matrix) (findStartPos matrix)

secondPart :: FilePath -> IO ()
secondPart file = do
  contents <- readFile file
  let ls = lines contents
  print ls
