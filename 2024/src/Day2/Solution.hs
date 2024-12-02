{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day2.Solution
  ( firstPart,
    secondPart,
  )
where

isSafeLine :: String -> Bool
isSafeLine line =
  let content = map read (words line)
   in isSafeList content 0 0

isSafeList :: [Int] -> Int -> Int -> Bool
isSafeList (el : l) 0 0 = isSafeList l el 0
isSafeList (el : l) n 0
  | el > n && el - n < 4 = isSafeList l el 1
  | el < n && n - el < 4 = isSafeList l el (-1)
  | otherwise = False
isSafeList (el : l) n state
  | el == n = False
  | state > 0 && el - n > 3 = False
  | state < 0 && n - el > 3 = False
  | state > 0 && el > n = isSafeList l el state
  | state < 0 && el < n = isSafeList l el state
  | otherwise = False
isSafeList [] _ _ = True

isSafeLine2 :: String -> Bool
isSafeLine2 line =
  let content = map read (words line)
   in helper content []
  where
    helper :: [Int] -> [Int] -> Bool
    helper [] _ = False
    helper (el : l) lprev =
      isSafeList (reverse lprev ++ l) 0 0 || helper l (el : lprev)

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  print $ sum $ map (\x -> if isSafeLine x then 1 else 0) (lines contents)

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  print $ sum $ map (\x -> if isSafeLine2 x then 1 else 0) (lines contents)
