-- module HaskellBowling.Calc (calcOne, calcTwo, calcThree) where

-- [Char]あるいは String を Int にする
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

-- XXX
-- XX9
-- X9/
-- X0/
-- 9/X
-- 9/9
-- 90-

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

-- score :: [(Int, Int, Int)] -> String
-- (concat [ "| " ++ show a ++ show b ++ show c ++ " " | [a:b:c]<- xs])
score ((a:b:[]):xs) = "|" ++ (calcTwo a b) ++ (score xs)
score ((a:b:c:[]):xs) = "|" ++ (calcThree a b c) ++ (score xs)
score [] = ""

main = do
  -- x <- getInts -- < 1 2 3 4 5 6
  -- print (intersperse ',' printBoard)  -- intercalate
  -- print x      -- > [1,2,3,4,5,6]
  let testData = [[1, 9], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [10, 0], [0,0,0]]
  print board
  print (score testData)
  print "Enxxd"

