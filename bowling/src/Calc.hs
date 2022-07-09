module Calc (calcOne, calcTwo, calcThree) where

calcOne :: Int -> String
calcOne first
  | first == 10 = "X"
  | otherwise = show first

calcTwo :: Int -> Int -> String
calcTwo first second
  | first == 10 = "X -"
  | sum' == 10 = show first ++ " " ++ "/"
  | otherwise = show first ++ " " ++ show second
  where sum' = first + second

calcThree :: Int -> Int -> Int -> String
calcThree first second third
  | sum' < 10 = show first ++ show second ++ "-"
  | isSpareSecond = (calcTwo first second) ++ (calcOne third)
  | isStrikeFirst && isStrikeSecond = "XX" ++ (calcOne third)
  | isStrikeFirst && not isSpareSecond = "X" ++ (calcTwo second third)
  where
    sum' = first + second
    isStrikeFirst = first == 10
    isStrikeSecond = isStrikeFirst && second == 10
    isSpareSecond = not isStrikeFirst && sum' == 10
