{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day3.Solution
  ( firstPart,
    secondPart,
  )
where

import Text.Regex.Posix ((=~))

getRestAndMatch :: (String, String, String, [String]) -> (String, [String])
getRestAndMatch (_, _, rest, match) = (rest, match)

sumMul :: String -> String -> Int
sumMul "" _ = 0
sumMul text pattern =
  let result = text =~ pattern :: (String, String, String, [String])
   in let restAndMatch = getRestAndMatch result
       in mul (snd restAndMatch) + sumMul (fst restAndMatch) pattern

sumMul2 :: String -> String -> String -> String -> Int
sumMul2 text pattern pattern2 pattern3 =
  helper text pattern pattern2 pattern3 True
  where
    helper :: String -> String -> String -> String -> Bool -> Int
    helper "" _ _ _ _ = 0
    helper text pattern pattern2 pattern3 True =
      let result = getRestAndMatch (text =~ pattern :: (String, String, String, [String]))
          result2 = getRestAndMatch (text =~ pattern2 :: (String, String, String, [String]))
          result3 = getRestAndMatch (text =~ pattern3 :: (String, String, String, [String]))
       in if length (fst result) > length (fst result3)
            then mul (snd result) + helper (fst result) pattern pattern2 pattern3 True
            else
              if length (fst result3) > length (fst result2)
                then helper (fst result2) pattern pattern2 pattern3 True
                else helper (fst result3) pattern pattern2 pattern3 False
    helper text pattern pattern2 pattern3 False =
      let result2 = getRestAndMatch (text =~ pattern2 :: (String, String, String, [String]))
       in helper (fst result2) pattern pattern2 pattern3 True

-- this case works if only provided list was empty
mul :: [String] -> Int
mul [] = 0
mul [el] = read el
mul (el : l) = read el * mul l

firstPart :: FilePath -> IO ()
firstPart filename = do
  contents <- readFile filename
  let regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
  print $ sumMul contents regex

secondPart :: FilePath -> IO ()
secondPart filename = do
  contents <- readFile filename
  let regex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"
  let regex2 = "do\\(\\)"
  let regex3 = "don't\\(\\)"
  print $ sumMul2 contents regex regex2 regex3
