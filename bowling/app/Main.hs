module Main where

import           Calc (calcOne, calcThree, calcTwo)

toInt :: (String -> Int)
toInt x = read x ::Int

--入力を受け取り、[Int]にする
getInts = do
  x <- getLine
  print x
  return (toInt x)


board :: String
board = (concat ["| " ++ x ++ " "| x <- scoreList])
  where scoreList = ["1","2","3","4","5","6","7","8","9","10","SUM"]


-- score :: [(Int, Int, Int)] -> String
-- (concat [ "| " ++ show a ++ show b ++ show c ++ " " | [a:b:c]<- xs])
score ((a:b:[]):xs)   = "|" ++ (calcTwo a b) ++ (score xs)
score ((a:b:c:[]):xs) = "|" ++ (calcThree a b c) ++ (score xs)
score []              = ""

main = do
  -- x <- getInts -- < 1 2 3 4 5 6
  -- print (intersperse ',' printBoard)  -- intercalate
  -- print x      -- > [1,2,3,4,5,6]
  let testData = [[1, 9], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [0,0,0]]
  print board
  print (score testData)
  print "Enxxd"
