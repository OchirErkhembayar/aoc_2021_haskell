{-# LANGUAGE TupleSections #-}

module Day4 (rund4) where

import Data.List.Split (splitOn, splitWhen)

type Cell = (Int, Bool)

type Board = [[Cell]]

updateBoards :: Int -> [Board] -> [Board]
updateBoards number = map (map updateRow)
  where
    updateRow = map (\(val, picked) -> if val == number then (val, True) else (val, picked))

boardWon :: Board -> Int -> Bool
boardWon board index
  | index == length board = False
  | isFull row || isFull column = True
  | otherwise = boardWon board $ index + 1
  where
    isFull :: [(Int, Bool)] -> Bool
    isFull [] = True
    isFull ((_, marked) : xs)
      | marked = isFull xs
      | otherwise = False
    row = board !! index
    column = map (!! index) board

filterWon :: [Board] -> [Board]
filterWon = filter (\board -> not $ boardWon board 0)

checkBoards :: [Board] -> Int -> Maybe Int
checkBoards [] _ = Nothing
checkBoards (board : rest) lastNumber = case checkBoard 0 of
  Just score -> Just score
  Nothing -> checkBoards rest lastNumber
  where
    checkBoard :: Int -> Maybe Int
    checkBoard index
      | index == length board = Nothing
      | otherwise = if boardWon board index then Just score else Nothing
      where
        score = lastNumber * foldr (\x acc -> acc + foldr (\y carry -> if not $ snd y then carry + fst y else carry) 0 x) 0 board

partTwo :: [Board] -> [Int] -> Int
partTwo [] _ = error "Winning board not found"
partTwo _ [] = error "Ran out of numbers before a winning board was found"
partTwo boards (number : rest)
  | length filteredBoards == 1 = partOne filteredBoards rest
  | otherwise = partTwo filteredBoards rest
  where
    filteredBoards = filterWon (updateBoards number boards)

partOne :: [Board] -> [Int] -> Int
partOne _ [] = error "Ran out of numbers before a winning board was found"
partOne boards (number : rest) = case checkBoards (updateBoards number boards) number of
  Just score -> score
  Nothing -> partOne (updateBoards number boards) rest

rund4 :: IO ()
rund4 = do
  contents <- readFile "./data/day4.txt"
  let allLines = lines contents
  let firstLine = map (\x -> read x :: Int) $ splitOn "," $ head allLines
  let restLines = map (map (\y -> read y :: Int) . words) $ tail $ tail allLines
  let boards = toBoards restLines
  print $ partTwo boards firstLine
  print $ partOne boards firstLine
  where
    toBoards :: [[Int]] -> [Board]
    toBoards numbers = mapBoards $ splitWhen null numbers
      where
        mapBoards :: [[[Int]]] -> [Board]
        mapBoards = map mapBoard
          where
            mapBoard :: [[Int]] -> Board
            mapBoard = map (map (,False))
