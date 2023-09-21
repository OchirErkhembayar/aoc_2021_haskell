module Modules.DayFour (rund4) where

import Data.List.Split (splitOn)

-- Input Parsing --
-- Split to lines
-- First line is just an arr
-- Next lines: array of arrays of tuples

-- Final shape --
-- List of Papers
-- Papers -> List of Rows
-- Rows -> List of Points
-- Points -> Tuple of (number, checked)
-- [ [ [(1, False), (25, True)], [(23, True), (56, False)] ] ]

rund4 :: IO ()
rund4 = do
  contents <- readFile "./data/day4t.txt"
  let allLines = lines contents
  let firstLine = map (\x -> read x :: Int) $ splitOn "," $ head allLines
  print $ map (\x -> toTuples x) $ map (\x -> toDigits x) $ map (\x -> words x) $ filter (\x -> not $ null x) $ tail allLines
  where
    toDigits :: [String] -> [Int]
    toDigits = map (\x -> read x :: Int)

    toTuples :: [Int] -> [(Int, Bool)]
    toTuples = map (\x -> (x, False))
  -- let otherLines = map (\x -> read x :: Int) $ map words $ tail allLines
  -- print otherLines
