module Day9 (rund9) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Pool = [GridPoint]

data GridPoint = GridPoint
  { location :: (Int, Int),
    height :: Int
  }
  deriving (Show, Eq)

intoPools :: [GridPoint] -> [Pool] -> [Pool]
intoPools gps pools = if null nonNines then pools else intoPools newGrid newPools
  where
    nonNines = filter (\gp -> height gp /= 9) gps
    (newPool, newGrid) = createPool gps (head nonNines) []
    newPools = newPool : pools

createPool :: [GridPoint] -> GridPoint -> Pool -> (Pool, [GridPoint])
createPool gps gp pool = newPoolAndGrid
  where
    newPoints = filter (\p -> height p /= 9) (findNeighbours gps gp)
    newGridPts = removePoint gps gp
    newPool = combinePoolsUniq pool newPoints
    newPoolAndGrid = foldr (\p (accPool, newPts) -> createPool newPts p accPool) (addIfContains newPool gp, newGridPts) newPoints

combinePoolsUniq :: Pool -> Pool -> Pool
combinePoolsUniq p1 p2 = p1 ++ filter (`notElem` p1) p2

addIfContains :: Pool -> GridPoint -> Pool
addIfContains pool gp = if gp `elem` pool then pool else gp : pool

findNeighbours :: [GridPoint] -> GridPoint -> [GridPoint]
findNeighbours gps gp = filter (adjacent gp) gps

removePoint :: [GridPoint] -> GridPoint -> [GridPoint]
removePoint gps gp = filter (\p -> location p /= location gp) gps

findLowPoints :: [GridPoint] -> [GridPoint]
findLowPoints gps =
  filter
    ( \gp ->
        isLowPoint
          gp
          (findAdjacents gps gp)
    )
    gps

isLowPoint :: GridPoint -> [GridPoint] -> Bool
isLowPoint gp = all (\p -> height p > height gp)

findAdjacents :: [GridPoint] -> GridPoint -> [GridPoint]
findAdjacents gps gp = filter (adjacent gp) gps

adjacent :: GridPoint -> GridPoint -> Bool
adjacent gp1 gp2 = beside || aboveOrBelow
  where
    (x1, y1) = location gp1
    (x2, y2) = location gp2
    beside = (abs (x1 - x2) == 1) && (y1 == y2)
    aboveOrBelow = (abs (y1 - y2) == 1) && (x1 == x2)

makeGridPoints :: [[(Int, Int, Int)]] -> [GridPoint]
makeGridPoints = concatMap (map (\(h, x, y) -> GridPoint (x, y) h))

addCoords :: [[Int]] -> [[(Int, Int, Int)]]
addCoords = addYCoords . addHeights
  where
    addHeights = map (\row -> zip row [0 :: Int ..])
    addYCoords = zipWith (\y row -> map (\(h, x) -> (h, x, y)) row) [0 :: Int ..]

getParsedInput :: IO [[Int]]
getParsedInput = do
  rawInput <- TIO.readFile "data/day9.txt"
  return $ map (map (\p -> read (T.unpack p) :: Int) . T.chunksOf 1) $ T.lines rawInput

rund9 :: IO ()
rund9 = do
  parsedInput <- addCoords <$> getParsedInput
  let gridPoints = makeGridPoints parsedInput
  let lowestPoints = findLowPoints gridPoints
  let partOne = sum $ map (\gp -> height gp + 1) lowestPoints
  print partOne
  let partTwo = product $ take 3 $ sortBy (comparing Data.Ord.Down) (map length $ intoPools gridPoints [])
  print partTwo
