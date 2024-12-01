import Control.Monad
import System.IO

main = do
  contents <- readFile "input.txt"
  let input = words contents
  let firstList = f1 input
  let secondList = f2 input
  let firstPartAnswer = sum $ diff $ pairMins firstList secondList
  let secondPartAnswer = secondPart firstList secondList
  print firstPartAnswer
  print secondPartAnswer

f1 :: [String] -> [Int]
f1 l = fHelper odd l 1

f2 :: [String] -> [Int]
f2 l = fHelper even l 1

fHelper :: (Int -> Bool) -> [String] -> Int -> [Int]
fHelper f (el : l) n = if f n then read el : fHelper f l (n + 1) else fHelper f l (n + 1)
fHelper _ [] _ = []

pairMins :: [Int] -> [Int] -> [(Int, Int)]
pairMins _ [] = []
pairMins [] _ = []
pairMins l1 l2 =
  let min1 = minimum l1
   in let min2 = minimum l2
       in (min1, min2) : pairMins (removeFromList min1 l1) (removeFromList min2 l2)

removeFromList :: Int -> [Int] -> [Int]
removeFromList n l = helper n l []
  where
    helper :: Int -> [Int] -> [Int] -> [Int]
    helper n (el : l) res
      | n == el = reverse res ++ l
      | otherwise = helper n l (el : res)
    helper _ [] res = reverse res

diff :: [(Int, Int)] -> [Int]
diff = map (\el -> abs $ uncurry (-) el)

apperanseCount :: Int -> [Int] -> Int
apperanseCount _ [] = 0
apperanseCount n (el : l) = if n == el then 1 + apperanseCount n l else apperanseCount n l

secondPart :: [Int] -> [Int] -> Int
secondPart [] [] = 0
secondPart _ [] = 0
secondPart [] _ = 0
secondPart (el : l1) l2 = apperanseCount el l2 * el + secondPart l1 l2
