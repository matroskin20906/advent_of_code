module Day5.Solution
  ( firstPart,
    secondPart,
  )
where

split :: String -> Char -> [String]
split [] _ = []
split ls del =
  helper ls del ""
  where
    helper :: String -> Char -> String -> [String]
    helper [] _ r = [reverse r]
    helper (el : l) d r =
      if el == d
        then reverse r : helper l d ""
        else helper l d (el : r)

existKey :: Int -> [(Int, [Int])] -> Bool
existKey _ [] = False
existKey k (el : m) =
  (k == fst el) || existKey k m

getValue :: Int -> [(Int, [Int])] -> [Int]
getValue _ [] = []
getValue k (el : m) =
  if k == fst el
    then snd el
    else getValue k m

delKey :: Int -> [(Int, [Int])] -> [(Int, [Int])]
delKey _ [] = []
delKey k (el : m) = if fst el == k then m else el : delKey k m

add :: [String] -> [(Int, [Int])] -> [(Int, [Int])]
add l m =
  if existKey (read (head l)) m
    then
      let k = read (head l)
          v = getValue k m
       in (k, read (l !! 1) : v) : delKey k m
    else (read (head l), [read (l !! 1)]) : m

haveSame :: (Eq a) => [a] -> [a] -> Bool
haveSame [] _ = False
haveSame _ [] = False
haveSame (el : l1) l2 = elem el l2 || haveSame l1 l2

valid :: String -> [(Int, [Int])] -> Bool
valid s r =
  let l = split s ','
   in helper l r []
  where
    helper :: [String] -> [(Int, [Int])] -> [Int] -> Bool
    helper [] _ _ = True
    helper (el : l) m p =
      let v = getValue (read el) m
       in (not (haveSame v p) && helper l m (read el : p))

getValidLines :: [String] -> [(Int, [Int])] -> [String]
getValidLines [] _ = []
getValidLines (el : l) m =
  if valid el m
    then el : getValidLines l m
    else getValidLines l m

getInvalidLines :: [String] -> [(Int, [Int])] -> [String]
getInvalidLines [] _ = []
getInvalidLines (el : l) m =
  if not $ valid el m
    then el : getInvalidLines l m
    else getInvalidLines l m

getMiddleElement :: String -> Int
getMiddleElement s =
  let l = split s ','
   in helper l (length l `div` 2) 0
  where
    helper :: [String] -> Int -> Int -> Int
    helper [] _ _ = 0
    helper (el : l) n i = if n == i then read el else helper l n (i + 1)

getMiddleElementInt :: [Int] -> Int
getMiddleElementInt l =
  helper l (length l `div` 2) 0
  where
    helper :: [Int] -> Int -> Int -> Int
    helper [] _ _ = 0
    helper (el : list) n i = if n == i then el else helper list n (i + 1)

buildMiddleElementsList :: [String] -> [Int]
buildMiddleElementsList = map getMiddleElement

buildMiddleElementsList2 :: [[Int]] -> [Int]
buildMiddleElementsList2 = map getMiddleElementInt

readRules :: [String] -> [String]
readRules [] = []
readRules (el : l) = if null el then [] else el : readRules l

processRules :: [String] -> [(Int, [Int])]
processRules [] = []
processRules rules =
  helper rules []
  where
    helper :: [String] -> [(Int, [Int])] -> [(Int, [Int])]
    helper [] r = r
    helper (el : l) r = helper l (add (split el '|') r)

readPageOrder :: [String] -> [String]
readPageOrder [] = []
readPageOrder l =
  helper l False
  where
    helper :: [String] -> Bool -> [String]
    helper [] _ = []
    helper (el : ls) False = helper ls (null el)
    helper (el : ls) True = el : helper ls True

firstPart :: FilePath -> IO ()
firstPart filename = do
  content <- readFile filename
  let ls = lines content
  let rules = readRules ls
  let pageOrder = readPageOrder ls
  let ruleMap = processRules rules
  print $ sum $ buildMiddleElementsList $ getValidLines pageOrder ruleMap

addValid :: Int -> [(Int, [Int])] -> [Int] -> [Int]
addValid el _ [] = [el]
addValid el r (elr : res) =
  let v = getValue el r
   in if haveSame v (elr : res)
        then elr : addValid el r res
        else el : (elr : res)

fixLine :: String -> [(Int, [Int])] -> [Int]
fixLine line rules =
  let list = map read (split line ',')
   in helper list rules []
  where
    helper :: [Int] -> [(Int, [Int])] -> [Int] -> [Int]
    helper [] _ res = res
    helper (el : list) r res =
      helper list r (addValid el r res)

fixLines :: [String] -> [(Int, [Int])] -> [[Int]]
fixLines [] _ = []
fixLines (line : ls) rules = fixLine line rules : fixLines ls rules

secondPart :: FilePath -> IO ()
secondPart filename = do
  content <- readFile filename
  let ls = lines content
  let rules = readRules ls
  let pageOrder = readPageOrder ls
  let ruleMap = processRules rules
  print $ sum $ buildMiddleElementsList2 $ fixLines (getInvalidLines pageOrder ruleMap) ruleMap
