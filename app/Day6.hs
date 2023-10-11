module Day6 (rund6) where

import Data.List.Split
import qualified Data.Map as M

progressDay :: M.Map Int Int -> M.Map Int Int
progressDay timers = M.fromListWith (+) $ processFish (M.toList timers)
  where
    processFish [] = []
    processFish ((timer, count) : xs) = case timer of
      0 -> [(6, count), (8, count)] ++ processFish xs
      _ -> (timer - 1, count) : processFish xs

simulate :: M.Map Int Int -> Int -> M.Map Int Int
simulate timers days = case days of
  0 -> timers
  _ -> simulate (progressDay timers) (days - 1)

initialiseData :: [Int] -> M.Map Int Int
initialiseData = foldr (\timer acc -> M.insertWith (+) timer 1 acc) $ M.fromList [(8, 0), (7, 0), (6, 0), (5, 0), (4, 0), (3, 0), (2, 0), (1, 0), (0, 0)]

rund6 :: IO ()
rund6 = do
  input <- readFile "./data/day6.txt"
  let timers = fmap (\x -> read x :: Int) $ splitOn "," $ filter (/= '\n') input
  let timersMap = initialiseData timers
  print $ sum $ M.elems $ simulate timersMap 256
  putStrLn "Hey"
