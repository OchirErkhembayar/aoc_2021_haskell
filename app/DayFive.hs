module DayFive where

import Data.List.Split (splitOn)
import qualified Data.Map as M

data Coords = Coords
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

data Line = Line Coords Coords deriving (Show, Eq)

extractCoords :: Coords -> (Int, Int)
extractCoords coords = (x coords, y coords)

makePoints :: Line -> [Coords]
makePoints (Line (Coords x1 y1) (Coords x2 y2))
  | horizontal y1 y2 = map (`Coords` y1) xRange
  | vertical x1 x2 = map (x1 `Coords`) yRange
  | otherwise = map (uncurry Coords) diagonalRange
  where
    diagonalRange = zip xRange yRange
    xRange = if x1 <= x2 then [x1 .. x2] else reverse [x2 .. x1]
    yRange = if y1 <= y2 then [y1 .. y2] else reverse [y2 .. y1]

horizontal :: Int -> Int -> Bool
horizontal y1 y2 = y1 == y2

vertical :: Int -> Int -> Bool
vertical x1 x2 = x1 == x2

diagonal :: Line -> Bool
diagonal (Line (Coords x1 y1) (Coords x2 y2)) = abs (x1 - x2) == abs (y1 - y2)

verticalOrHorizontal :: Line -> Bool
verticalOrHorizontal (Line (Coords x1 y1) (Coords x2 y2)) = vertical x1 x2 || horizontal y1 y2

verticalOrHorizontalOrDiagonal :: Line -> Bool
verticalOrHorizontalOrDiagonal line = verticalOrHorizontal line || diagonal line

parseLine :: String -> Line
parseLine str = combine $ map parse $ splitOn " -> " str
  where
    parse string =
      case splitOn "," string of
        [xCoord, yCoord] -> Coords (read xCoord :: Int) (read yCoord :: Int)
        _ -> error "Bad input"
    combine [xCoord, yCoord] = Line xCoord yCoord
    combine _ = error "Bad input"

mapCoords :: [Coords] -> M.Map (Int, Int) Int
mapCoords = foldl (\acc point -> M.insertWith (+) (extractCoords point) (1 :: Int) acc) M.empty

answer :: [Line] -> (Line -> Bool) -> Int
answer lines' filterStrat = M.size $ M.filter (>= 2) $ mapCoords $ concatMap makePoints $ filter filterStrat lines'

rund5 :: IO ()
rund5 = do
  contents <- readFile "./data/day5.txt"
  let allLines = map parseLine $ lines contents
  let partOne = answer allLines verticalOrHorizontal
  let partTwo = answer allLines verticalOrHorizontalOrDiagonal
  print partOne
  print partTwo
  putStrLn "Lol"

