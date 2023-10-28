module Day9 (rund9) where

import Data.List.Split (splitOn)

type Height = Int

type Position = Int

type Point = (Height, Position)

type Grid = [[Point]]

type Row = Int

findLowestPoints :: Grid -> [Point]
findLowestPoints grid = foldl (\acc row -> acc ++ filterRow grid row (grid !! row)) [] [0 .. (length grid - 1)]

isLowPoint :: Point -> [Point] -> Bool
isLowPoint point = all (> point)

filterRow :: Grid -> Row -> [Point] -> [Point]
filterRow grid row = filter (\point -> isLowPoint point $ findAdjacents grid row point)

-- findAdjacentsForRow :: Grid -> Row -> [Point]
-- findAdjacentsForRow grid row =
--     let points = grid!!row
--     in foldl (\acc point -> acc ++ findAdjacents grid row point) [] points

findAdjacents :: Grid -> Row -> Point -> [Point]
findAdjacents grid row (height, col)
  | row == 0 = [left, right, bottom]
  | row == lastRow = [left, right, top]
  | otherwise = [left, right, top, bottom]
  where
    lastRow = length grid - 1
    currentRow = grid !! row
    left = if col > 0 then currentRow !! (col - 1) else (height + 1, col)
    right = if col < (length currentRow - 1) then currentRow !! (col + 1) else (height + 1, col)
    top = if row > 0 then grid !! (row - 1) !! col else (height + 1, col)
    bottom = if row < (length grid - 1) then grid !! (row + 1) !! col else (height + 1, col)

rund9 :: IO ()
rund9 = do
  rawInput <- readFile "data/day9.txt"
  let grid = map (\row -> zip ((map (\x -> read x :: Int) . tail . splitOn "") row) [0 :: Int ..]) $ lines rawInput
  let lowestPoints = findLowestPoints grid
  let partOne = sum $ map (\(height, _) -> height + 1) lowestPoints
  print partOne
  putStrLn "Hello, World!"
