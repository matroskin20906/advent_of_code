module Main where

import Day1.Solution as S1
import Day2.Solution as S2
import Day3.Solution as S3
import Day4.Solution as S4
import Day5.Solution as S5
import Day6.Solution as S6
import Day7.Solution as S7
import Day8.Solution as S8
import Day9.Solution as S9

main :: IO ()
main = do
  S1.firstPart "inputs/input1.txt"
  S1.secondPart "inputs/input1.txt"
  S2.firstPart "inputs/input2.txt"
  S2.secondPart "inputs/input2.txt"
  S3.firstPart "inputs/input3.txt"
  S3.secondPart "inputs/input3.txt"
  S4.firstPart "inputs/input4.txt"
  S4.secondPart "inputs/input4.txt"
  S5.firstPart "inputs/input5.txt"
  S5.secondPart "inputs/input5.txt"
  S6.firstPart "inputs/input6.test"
  S6.secondPart "inputs/input6.test"
  S7.firstPart "inputs/input7.txt"
  S7.secondPart "inputs/input7.txt"
  S8.firstPart "inputs/input8.txt"
  S8.secondPart "inputs/input8.txt"
  S9.firstPart "inputs/input9.test"
  S9.secondPart "inputs/input9.test"
