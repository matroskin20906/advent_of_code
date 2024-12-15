module Day15.Solution
  ( firstPart,
    secondPart,
  )
where

import GHC.Arr (Array, array, assocs, (!), (//))

type Vector = (Int, Int)

sumVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

readMatrix :: [String] -> [[Char]]
readMatrix [] = []
readMatrix ("" : _) = []
readMatrix (l : tl) = l : readMatrix tl

readSteps :: [String] -> [Char]
readSteps ls =
  helper ls False
  where
    helper [] _ = []
    helper (s : tl) active
      | s == "" = helper tl True
      | not active = helper tl active
      | otherwise = s ++ helper tl active

toVector :: Char -> Vector
toVector '^' = (0, -1)
toVector '<' = (-1, 0)
toVector '>' = (1, 0)
toVector 'v' = (0, 1)
toVector _ = (0, 0)

value :: Array (Int, Int) Char -> Vector -> Char
value matrix (x, y) = matrix ! (x, y)

updateMatrix :: Array (Int, Int) Char -> Vector -> Char -> Array (Int, Int) Char
updateMatrix matrix (x, y) newValue = matrix // [((x, y), newValue)]

makeSteps :: Array (Int, Int) Char -> (Int, Int) -> [Char] -> Array (Int, Int) Char
makeSteps matrix _ [] = matrix
makeSteps matrix robotPos (step : tl) =
  let newPos = makeStep matrix robotPos step
   in makeSteps (snd newPos) (fst newPos) tl

makeStep :: Array (Int, Int) Char -> (Int, Int) -> Char -> ((Int, Int), Array (Int, Int) Char)
makeStep matrix robotPos step =
  let vector = toVector step
      newRobotPos = sumVectors robotPos vector
      token = value matrix robotPos
   in if value matrix newRobotPos == '.'
        then (newRobotPos, updateMatrix (updateMatrix matrix robotPos '.') newRobotPos token)
        else
          if value matrix newRobotPos == '#'
            then (robotPos, matrix)
            else
              if value matrix newRobotPos == 'O'
                then
                  let newMatrix = makeStep matrix newRobotPos step
                   in if value (snd newMatrix) newRobotPos == '.'
                        then (newRobotPos, updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token)
                        else (robotPos, snd newMatrix)
                else (robotPos, matrix)

toArray :: [[a]] -> Array (Int, Int) a
toArray xss = array ((0, 0), (m - 1, n - 1)) [((i, j), xss !! j !! i) | i <- [0 .. m - 1], j <- [0 .. n - 1]]
  where
    n = length xss
    m = length (head xss)

getRobotPos :: Array (Int, Int) Char -> (Int, Int)
getRobotPos matrix =
  helper (assocs matrix)
  where
    helper [] = (-1, -1)
    helper (c : tl)
      | snd c == '@' = fst c
      | otherwise = helper tl

sumGoodsPositions :: Array (Int, Int) Char -> Int
sumGoodsPositions matrix =
  helper (assocs matrix)
  where
    helper [] = 0
    helper (c : tl)
      | snd c == 'O' = fst (fst c) + snd (fst c) * 100 + helper tl
      | otherwise = helper tl

firstPart :: FilePath -> IO ()
firstPart file = do
  contents <- readFile file
  let ls = lines contents
  let matrix = toArray (readMatrix ls)
  let steps = readSteps ls
  print $ sumGoodsPositions $ makeSteps matrix (getRobotPos matrix) steps

modifyMatrix :: [[Char]] -> [[Char]]
modifyMatrix = map modifyRow

modifyRow :: [Char] -> [Char]
modifyRow [] = []
modifyRow (r : tl)
  | r == '.' = '.' : '.' : modifyRow tl
  | r == '#' = '#' : '#' : modifyRow tl
  | r == '@' = '@' : '.' : modifyRow tl
  | otherwise = '[' : ']' : modifyRow tl

makeSteps2 :: Array (Int, Int) Char -> (Int, Int) -> [Char] -> Array (Int, Int) Char
makeSteps2 matrix _ [] = matrix
makeSteps2 matrix robotPos (step : tl) =
  let newPos = makeStep2 matrix robotPos step
   in makeSteps2 (snd newPos) (fst newPos) tl

makeStep2 :: Array (Int, Int) Char -> (Int, Int) -> Char -> ((Int, Int), Array (Int, Int) Char)
makeStep2 matrix robotPos step =
  let vector = toVector step
      newRobotPos = sumVectors robotPos vector
      token = value matrix robotPos
   in if step == '<' || step == '>'
        then
          if value matrix newRobotPos == '.'
            then (newRobotPos, updateMatrix (updateMatrix matrix robotPos '.') newRobotPos token)
            else
              if value matrix newRobotPos == '#'
                then (robotPos, matrix)
                else
                  if value matrix newRobotPos == '[' || value matrix newRobotPos == ']'
                    then
                      let newMatrix = makeStep2 matrix newRobotPos step
                       in if value (snd newMatrix) newRobotPos == '.'
                            then (newRobotPos, updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token)
                            else (robotPos, snd newMatrix)
                    else (robotPos, matrix)
        else
          if token == '@'
            then
              if value matrix newRobotPos == '.'
                then (newRobotPos, updateMatrix (updateMatrix matrix robotPos '.') newRobotPos token)
                else
                  ( if (value matrix newRobotPos == '[') || (value matrix newRobotPos == ']')
                      then
                        ( let newMatrix = makeStep2 matrix newRobotPos step
                           in if value (snd newMatrix) newRobotPos == '.'
                                then (newRobotPos, updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token)
                                else (robotPos, snd newMatrix)
                        )
                      else (robotPos, matrix)
                  )
            else
              if token == '['
                then
                  if value matrix newRobotPos == '.' && value matrix (sumVectors newRobotPos (1, 0)) == '.'
                    then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix matrix robotPos '.') newRobotPos token) (sumVectors robotPos (1, 0)) '.') (sumVectors newRobotPos (1, 0)) ']')
                    else
                      ( if (value matrix newRobotPos == '[' && value matrix (sumVectors newRobotPos (1, 0)) == ']') || (value matrix newRobotPos == ']' && value matrix (sumVectors newRobotPos (1, 0)) == '.')
                          then
                            ( let newMatrix = makeStep2 matrix newRobotPos step
                               in if value (snd newMatrix) newRobotPos == '.'
                                    then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token) (sumVectors robotPos (1, 0)) '.') (sumVectors newRobotPos (1, 0)) ']')
                                    else (robotPos, matrix)
                            )
                          else
                            ( if value matrix (sumVectors newRobotPos (1, 0)) == '[' && value matrix newRobotPos == '.'
                                then
                                  let newMatrix = makeStep2 matrix (sumVectors newRobotPos (1, 0)) step
                                   in if value (snd newMatrix) (sumVectors newRobotPos (1, 0)) == '.'
                                        then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token) (sumVectors robotPos (1, 0)) '.') (sumVectors newRobotPos (1, 0)) ']')
                                        else (robotPos, matrix)
                                else
                                  if value matrix newRobotPos == ']' && value matrix (sumVectors newRobotPos (1, 0)) == '['
                                    then
                                      let newMatrix = makeStep2 (snd (makeStep2 matrix (sumVectors newRobotPos (1, 0)) step)) newRobotPos step
                                       in if value (snd newMatrix) newRobotPos == '.' && value (snd newMatrix) (sumVectors newRobotPos (1, 0)) == '.'
                                            then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token) (sumVectors robotPos (1, 0)) '.') (sumVectors newRobotPos (1, 0)) ']')
                                            else (robotPos, matrix)
                                    else (robotPos, matrix)
                            )
                      )
                else
                  if value matrix newRobotPos == '.' && value matrix (sumVectors newRobotPos (-1, 0)) == '.'
                    then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix matrix robotPos '.') newRobotPos token) (sumVectors robotPos (-1, 0)) '.') (sumVectors newRobotPos (-1, 0)) '[')
                    else
                      ( if (value matrix newRobotPos == '[' && value matrix (sumVectors newRobotPos (-1, 0)) == '.') || (value matrix newRobotPos == ']' && value matrix (sumVectors newRobotPos (-1, 0)) == '[')
                          then
                            ( let newMatrix = makeStep2 matrix newRobotPos step
                               in if value (snd newMatrix) newRobotPos == '.'
                                    then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token) (sumVectors robotPos (-1, 0)) '.') (sumVectors newRobotPos (-1, 0)) '[')
                                    else (robotPos, matrix)
                            )
                          else
                            ( if value matrix newRobotPos == '[' && value matrix (sumVectors newRobotPos (-1, 0)) == ']'
                                then
                                  let newMatrix = makeStep2 (snd (makeStep2 matrix (sumVectors newRobotPos (-1, 0)) step)) newRobotPos step
                                   in if value (snd newMatrix) newRobotPos == '.' && value (snd newMatrix) (sumVectors newRobotPos (-1, 0)) == '.'
                                        then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token) (sumVectors robotPos (-1, 0)) '.') (sumVectors newRobotPos (-1, 0)) '[')
                                        else (robotPos, matrix)
                                else
                                  if value matrix newRobotPos == '.' && value matrix (sumVectors newRobotPos (-1, 0)) == ']'
                                    then
                                      let newMatrix = makeStep2 matrix (sumVectors newRobotPos (-1, 0)) step
                                       in if value (snd newMatrix) (sumVectors newRobotPos (-1, 0)) == '.'
                                            then (newRobotPos, updateMatrix (updateMatrix (updateMatrix (updateMatrix (snd newMatrix) robotPos '.') newRobotPos token) (sumVectors robotPos (-1, 0)) '.') (sumVectors newRobotPos (-1, 0)) '[')
                                            else (robotPos, matrix)
                                    else (robotPos, matrix)
                            )
                      )

sumGoodsPositions2 :: Array (Int, Int) Char -> Int
sumGoodsPositions2 matrix =
  helper (assocs matrix)
  where
    helper [] = 0
    helper (c : tl)
      | snd c == '[' = fst (fst c) + snd (fst c) * 100 + helper tl
      | otherwise = helper tl

secondPart :: FilePath -> IO ()
secondPart file = do
  contents <- readFile file
  let ls = lines contents
  let modifiedInputMatrix = modifyMatrix (readMatrix ls)
  let matrix = toArray modifiedInputMatrix
  let steps = readSteps ls
  let afterSteps = makeSteps2 matrix (getRobotPos matrix) steps
  print $ sumGoodsPositions2 afterSteps
