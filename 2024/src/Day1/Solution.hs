module Day1.Solution
  ( firstPart,
    secondPart,
  )
where

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

countOccurrences :: Int -> [Int] -> Int
countOccurrences _ [] = 0
countOccurrences n (el : l) = if n == el then 1 + countOccurrences n l else countOccurrences n l

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let input = words contents
  let firstList = f1 input
  let secondList = f2 input
  print $ sum $ diff $ pairMins firstList secondList

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let input = words contents
  let firstList = f1 input
  let secondList = f2 input
  print $ secondPartHelper firstList secondList

secondPartHelper :: [Int] -> [Int] -> Int
secondPartHelper [] [] = 0
secondPartHelper _ [] = 0
secondPartHelper [] _ = 0
secondPartHelper (el : l1) l2 = countOccurrences el l2 * el + secondPartHelper l1 l2
