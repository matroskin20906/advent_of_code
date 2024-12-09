module Day9.Solution
  ( firstPart,
    secondPart,
  )
where

import Data.Char (digitToInt)

makeStringList :: String -> Int -> [String]
makeStringList _ 0 = []
makeStringList s i = s : makeStringList s (i - 1)

makeBlocks :: String -> [String]
makeBlocks "" = []
makeBlocks l =
  helper l 0 0
  where
    helper :: String -> Int -> Int -> [String]
    helper "" _ _ = []
    helper (c : tl) i blockId =
      if even i
        then makeStringList (show blockId) (digitToInt c) ++ helper tl (i + 1) (blockId + 1)
        else makeStringList "." (digitToInt c) ++ helper tl (i + 1) blockId

smushMemory :: [String] -> [String]
smushMemory s =
  helper s (reverse s)
  where
    helper :: [String] -> [String] -> [String]
    helper [] [] = []
    helper [] _ = []
    helper _ [] = []
    helper (sel : stl) (rsel : rstl)
      | (sel == ".") && (rsel /= ".") = rsel : helper stl rstl
      | (sel == ".") && (rsel == ".") = helper (sel : stl) rstl
      | (sel /= ".") && (rsel /= ".") = sel : helper stl (rsel : rstl)
      | (sel /= ".") && (rsel == ".") = sel : helper stl rstl
      | otherwise = []

calcChecksum :: [String] -> Int
calcChecksum s =
  helper s 0
  where
    helper :: [String] -> Int -> Int
    helper [] _ = 0
    helper ("." : tl) i = 0 + helper tl (i + 1)
    helper (c : tl) i = read c * i + helper tl (i + 1)

numberCount :: [String] -> Int
numberCount [] = 0
numberCount (c : tl) = if c == "." then numberCount tl else 1 + numberCount tl

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let line = head $ lines contents
  let blocks = makeBlocks line
  print $ calcChecksum $ take (numberCount blocks) (smushMemory blocks)

processTwoLists :: [String] -> [String] -> [[String]]
processTwoLists list1 list2
  | length list1 == length list2 = [list2]
  | length list1 > length list2 = [list2, makeStringList (head list1) (length list1 - length list2)]

removeFromList :: [[String]] -> [String] -> [[String]]
removeFromList l p =
  helper l p
  where
    helper :: [[String]] -> [String] -> [[String]]
    helper [] _ = []
    helper (el : tl) part =
      if el == p
        then makeStringList "." (length part) : helper tl part
        else el : helper tl part

processOneGroup :: [[String]] -> [String] -> Int -> [[String]]
processOneGroup [] _ _ = []
processOneGroup list p w =
  helper list p w 0 []
  where
    helper :: [[String]] -> [String] -> Int -> Int -> [[String]] -> [[String]]
    helper [] _ _ _ b = reverse b
    helper (el : tl) part i n b
      | head el == "." && length el == length part && i > n = reverse b ++ [part] ++ removeFromList tl part
      | head el == "." && length el > length part && i > n = reverse b ++ [part, makeStringList "." (length el - length part)] ++ removeFromList tl part
      | otherwise = helper tl part i (n + 1) (el : b)

mergeNodes :: [[String]] -> [[String]]
mergeNodes [] = []
mergeNodes (el : el2 : tl)
  | head el /= "." = el : mergeNodes (el2 : tl)
  | head el == "." && head el2 == "." = mergeNodes (makeStringList "." (length el + length el2) : tl)
mergeNodes (el : tl) = el : mergeNodes tl

smushMemory2 :: [[String]] -> [[String]]
smushMemory2 s =
  helper s (reverse s) (length s - 1)
  where
    helper :: [[String]] -> [[String]] -> Int -> [[String]]
    helper res _ 0 = res
    helper [] [] _ = []
    helper [] _ _ = []
    helper res [] _ = res
    -- we need to iterate throw all s trying to add rsel. if we can we must make new list from s and go to new rsel.
    -- if we cannot then go to next rsel without list modification
    helper ss (rsel : rstl) i =
      let processed = processOneGroup ss rsel i
          merged = mergeNodes processed
       in helper merged rstl (findInList merged (head rstl))

findInList :: [[String]] -> [String] -> Int
findInList [] _ = 0
findInList (el : tl) el2
  | el == el2 = 0
  | el /= el2 = 1 + findInList tl el2

makeBlocksMatrix :: [String] -> [[String]]
makeBlocksMatrix blocks =
  helper blocks []
  where
    helper :: [String] -> [String] -> [[String]]
    helper [] res = [res]
    helper (c : tl) [] = helper tl [c]
    helper (c : tl) (r : res) =
      if c == r
        then helper tl (c : r : res)
        else (r : res) : helper (c : tl) []

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let line = head $ lines contents
  let blocks = makeBlocks line
  -- print $ concat $ smushMemory2 $ makeBlocksMatrix blocks
  print $ calcChecksum $ concat $ smushMemory2 $ makeBlocksMatrix blocks
