module Main where

import Day1.Solution as S1
import Day2.Solution as S2
import Day3.Solution as S3

main :: IO ()
main = do
  S1.firstPart "inputs/input1.txt"
  S1.secondPart "inputs/input1.txt"
  S2.firstPart "inputs/input2.txt"
  S2.secondPart "inputs/input2.txt"
  S3.firstPart "inputs/input3.txt"
  S3.secondPart "inputs/input3.txt"
