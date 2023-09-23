module DayFour (rund4, toBoards) where

import Data.List.Split (splitOn)

toBoards :: [[Int]] -> [[(Int, Bool)]] -> [[[(Int, Bool)]]]-> [[[(Int, Bool)]]]
toBoards [] current acc = acc ++ [current]
toBoards (row:rows) current acc
    | length row == 0 = toBoards rows [] $ acc ++ [current]
    | otherwise = toBoards rows (current++[map (\x -> (x, False)) row]) acc

updateBoards :: Int -> [[[(Int, Bool)]]] -> [[[(Int, Bool)]]]
updateBoards number boards = map (\board -> map updateRow board) boards
    where
        updateRow row = map (\(val, picked) -> if val == number then (val, True) else (val, picked)) row

boardWon :: [[(Int, Bool)]] -> Int -> Bool
boardWon board index
    | index == (length board) = False
    | isFull row || isFull column = True
    | otherwise = boardWon board $ index + 1
        where
            isFull :: [(Int, Bool)] -> Bool
            isFull [] = True
            isFull ((_, marked):xs)
                | marked = isFull xs
                | otherwise = False
            row = board!!index
            column = foldr (\r acc -> r!!index : acc) [] board

filterWon :: [[[(Int, Bool)]]] -> [[[(Int, Bool)]]]
filterWon boards = filter (\board -> not $ boardWon board 0) boards

checkBoards :: [[[(Int, Bool)]]] -> Int -> Maybe Int
checkBoards [] _ = Nothing
checkBoards (board:rest) lastNumber = case checkBoard 0 of
    Just score -> Just score
    Nothing -> checkBoards rest lastNumber
    where
        checkBoard :: Int -> Maybe Int
        checkBoard index
            | index == (length board) = Nothing
            | otherwise = if boardWon board index then Just score else Nothing
                where
                    score = lastNumber * (foldr (\x acc -> acc + foldr (\y carry -> if (not $ snd y) then carry + fst y else carry) 0 x) 0 board)

winLast :: [[[(Int, Bool)]]] -> [Int] -> Int
winLast [] _ = error $ "Winning board not found"
winLast _ [] = error $ "Ran out of numbers before a winning board was found"
winLast boards (number:rest)
    | length filteredBoards == 1 = runGame filteredBoards rest
    | otherwise = winLast filteredBoards rest
        where
            filteredBoards = filterWon (updateBoards number boards)

runGame :: [[[(Int, Bool)]]] -> [Int] -> Int
runGame _ [] = error "Ran out of numbers before a winning board was found"
runGame boards (number:rest) = case checkBoards (updateBoards number boards) number of
    Just score -> score
    Nothing -> runGame (updateBoards number boards) rest

rund4 :: IO ()
rund4 = do
  contents <- readFile "./data/day4.txt"
  let allLines = lines contents
  let firstLine = map (\x -> read x :: Int) $ splitOn "," $ head allLines
  let restLines = map (\x -> map(\y -> read y :: Int) x) $ map (\x -> words x) $ tail $ tail allLines
  let boards = toBoards restLines [] []
  print $ winLast boards firstLine
  print $ runGame boards firstLine

