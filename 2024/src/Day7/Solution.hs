module Day7.Solution
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

prepareData :: [String] -> [(Int, [Int])]
prepareData [] = []
prepareData (el : tl) =
  let eqAndNums = split el ':'
   in (read (head eqAndNums), map read (filter (/= "") (split (eqAndNums !! 1) ' '))) : prepareData tl

removeTwoElFromList :: (Eq a) => a -> a -> [a] -> [a]
removeTwoElFromList _ _ [] = []
removeTwoElFromList el1 el2 list =
  helper el1 el2 False False list
  where
    helper _ _ _ _ [] = []
    helper a b firstRemoved secondRemoved (el : l)
      | el == a && not firstRemoved = helper a b True secondRemoved l
      | el == b && not secondRemoved = helper a b firstRemoved True l
      | otherwise = el : helper a b firstRemoved secondRemoved l

valid :: (Int, [Int]) -> Bool
valid (_, []) = False
valid (res, [res2]) = res == res2
valid (res, lmts) =
  let headEl = head lmts
      secondEl = lmts !! 1
   in ( valid (res, (headEl + secondEl) : removeTwoElFromList headEl secondEl lmts)
          || valid (res, (headEl * secondEl) : removeTwoElFromList headEl secondEl lmts)
      )

analyzeData :: [(Int, [Int])] -> Int
analyzeData [] = 0
analyzeData (el : tl) =
  if valid el then fst el + analyzeData tl else analyzeData tl

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let ls = lines contents
  print $ analyzeData $ prepareData ls

multByTenPower :: Int -> Int -> Int
multByTenPower x 0 = x
multByTenPower x y = multByTenPower (x * 10) (y - 1)

valid2 :: (Int, [Int]) -> Bool
valid2 (_, []) = False
valid2 (res, [res2]) = res == res2
valid2 (res, lmts) =
  let headEl = head lmts
      secondEl = lmts !! 1
   in ( valid2 (res, (headEl + secondEl) : removeTwoElFromList headEl secondEl lmts)
          || valid2 (res, (headEl * secondEl) : removeTwoElFromList headEl secondEl lmts)
          || valid2 (res, (multByTenPower headEl (length (show secondEl)) + secondEl) : removeTwoElFromList headEl secondEl lmts)
      )

analyzeData2 :: [(Int, [Int])] -> Int
analyzeData2 [] = 0
analyzeData2 (el : tl) =
  if valid2 el then fst el + analyzeData2 tl else analyzeData2 tl

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let ls = lines contents
  print $ analyzeData2 $ prepareData ls
