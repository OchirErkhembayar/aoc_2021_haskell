module Day7 (rund7) where

import Data.List.Split (splitOn)

bestPosition :: [Int] -> Int
bestPosition crabs = foldr (min . fuelUsage crabs) (fuelUsage crabs firstCrab) restCrabs
  where
    firstCrab : restCrabs = [minimum crabs .. maximum crabs]

fuelUsage :: [Int] -> Int -> Int
fuelUsage crabs target =
  let cost distance = div (distance * (distance + 1)) 2
   in foldr (\crab acc -> acc + cost (abs (crab - target))) 0 crabs

rund7 :: IO ()
rund7 = do
  input <- readFile "data/day7.txt"
  let crabs = fmap (\x -> read x :: Int) $ splitOn "," $ filter (/= '\n') input
  print $ bestPosition crabs
  putStrLn "crab rave"
