module Day13.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.Fixed (mod')
import Text.Regex.Posix ((=~))

splitToMachines [] = []
splitToMachines ls =
  helper ls []
  where
    helper [] l = [reverse l]
    helper (l : tl) a
      | l == "" = reverse a : helper tl []
      | otherwise = helper tl (l : a)

matches :: (String, String, String, [String]) -> [String]
matches (_, _, _, ms) = ms

machineToInts :: [String] -> [(Int, Int)]
machineToInts [] = []
machineToInts (l : tl) =
  let regex = "([0-9]+)[^0-9]*([0-9]+)"
      coords = matches (l =~ regex :: (String, String, String, [String]))
   in if length coords > 1
        then (read (head coords), read (last coords)) : machineToInts tl
        else machineToInts tl

machinesToInts [] = []
machinesToInts (machine : tl) = machineToInts machine : machinesToInts tl

multVector :: (Int, Int) -> Int -> (Int, Int)
multVector (x, y) scalar = (x * scalar, y * scalar)

sumVectors :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

findWinAlgs :: [(Int, Int)] -> [(Int, Int)]
findWinAlgs machine =
  helper (head machine) (machine !! 1) (machine !! 2) 100
  where
    helper _ _ _ (-1) = []
    helper a b win count =
      findWinAlgsFrozeB a b win 100 count ++ findWinAlgsFrozeA a b win count 100 ++ helper a b win (count - 1)

findWinAlgsFrozeA _ _ _ _ (-1) = []
findWinAlgsFrozeA a b win ca cb =
  if sumVectors (multVector a ca) (multVector b cb) == win
    then (ca, cb) : findWinAlgsFrozeA a b win ca (cb - 1)
    else findWinAlgsFrozeA a b win ca (cb - 1)

findWinAlgsFrozeB _ _ _ (-1) _ = []
findWinAlgsFrozeB a b win ca cb =
  if sumVectors (multVector a ca) (multVector b cb) == win
    then (ca, cb) : findWinAlgsFrozeB a b win (ca - 1) cb
    else findWinAlgsFrozeB a b win (ca - 1) cb

findWinAlgsAll [] = []
findWinAlgsAll (machine : tl) = findWinAlgs machine : findWinAlgsAll tl

calcPrice (a, b) = if a < 0 || b < 0 then 0 else a * 3 + b

toMinPrice :: [(Int, Int)] -> Int
toMinPrice [] = 0
toMinPrice strats = minimum $ map calcPrice strats

firstPart :: FilePath -> IO ()
firstPart file = do
  contents <- readFile file
  let ls = lines contents
  print $ sum $ map toMinPrice (findWinAlgsAll $ machinesToInts $ splitToMachines ls)

addModsPart2 [] = []
addModsPart2 [l] = [sumVectors l (10000000000000, 10000000000000)]
addModsPart2 (l : tl) = l : addModsPart2 tl

addModsPart2All [] = []
addModsPart2All (machine : machines) = addModsPart2 machine : addModsPart2All machines

det (x1, y1) (x2, y2) =
  x1 * y2 - x2 * y1

processOneMachinePart2 machine =
  let a = head machine
      b = machine !! 1
      win = machine !! 2
      d = det a b
   in if d == 0
        then 0
        else
          let ca =
                ( fromIntegral (snd b) * fromIntegral (fst win)
                    - fromIntegral (fst b) * fromIntegral (snd win)
                )
                  / fromIntegral d
              cb =
                ( fromIntegral (-snd a) * fromIntegral (fst win)
                    + fromIntegral (fst a) * fromIntegral (snd win)
                )
                  / fromIntegral d
           in if mod' ca 1 == 0 && mod' cb 1 == 0 then calcPrice (round ca, round cb) else 0

processMachinesPart2 [] = []
processMachinesPart2 (machine : tl) = processOneMachinePart2 machine : processMachinesPart2 tl

secondPart :: FilePath -> IO ()
secondPart file = do
  contents <- readFile file
  let ls = lines contents
  print $
    sum $
      processMachinesPart2 (addModsPart2All $ machinesToInts $ splitToMachines ls)
