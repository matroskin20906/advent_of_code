module Main where

import Day1.Solution as S1
import Day10.Solution as S10
import Day11.Solution as S11
import Day12.Solution as S12
import Day13.Solution as S13
import Day2.Solution as S2
import Day3.Solution as S3
import Day4.Solution as S4
import Day5.Solution as S5
import Day6.Solution as S6
import Day7.Solution as S7
import Day8.Solution as S8
import Day9.Solution as S9
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1"] -> do
      S1.firstPart "inputs/input1.txt"
      S1.secondPart "inputs/input1.txt"
    ["2"] -> do
      S2.firstPart "inputs/input2.txt"
      S2.secondPart "inputs/input2.txt"
    ["3"] -> do
      S3.firstPart "inputs/input3.txt"
      S3.secondPart "inputs/input3.txt"
    ["4"] -> do
      S4.firstPart "inputs/input4.txt"
      S4.secondPart "inputs/input4.txt"
    ["5"] -> do
      S5.firstPart "inputs/input5.txt"
      S5.secondPart "inputs/input5.txt"
    ["6"] -> do
      S6.firstPart "inputs/input6.test"
      S6.secondPart "inputs/input6.test"
    ["7"] -> do
      S7.firstPart "inputs/input7.txt"
      S7.secondPart "inputs/input7.txt"
    ["8"] -> do
      S8.firstPart "inputs/input8.txt"
      S8.secondPart "inputs/input8.txt"
    ["9"] -> do
      S9.firstPart "inputs/input9.test"
      S9.secondPart "inputs/input9.test"
    ["10"] -> do
      S10.firstPart "inputs/input10.txt"
      S10.secondPart "inputs/input10.txt"
    ["11"] -> do
      S11.firstPart "inputs/input11.txt"
      S11.secondPart "inputs/input11.txt"
    ["12"] -> do
      S12.firstPart "inputs/input12.txt"
      S12.secondPart "inputs/input12.txt"
    ["13"] -> do
      S13.firstPart "inputs/input13.txt"
      S13.secondPart "inputs/input13.txt"
    _ -> print "this solution does not exists"
