module Modules.DayThree (rund3) where

import Data.Char (digitToInt)

type Index = Int
type Bits = [Int]

mostCommonBits :: [Bits] -> Bits
mostCommonBits (first : xs) = map (\(x, y) -> if x > y then 0 else 1) $ foldr (addTuples . tupleIze) (tupleIze first) xs
  where
    tupleIze = map (\x -> if x == 0 then (1, 0) else (0, 1))
    addTuples = zipWith addTuple
      where
        addTuple (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

o2SRating :: [Bits] -> Index -> Bits
o2SRating xs index
  | length xs == 1 = head xs
  | otherwise = o2SRating (filter (\ys -> ys !! index == mostCommonBits xs !! index) xs) (index + 1)

epsilonRating :: [Bits] -> Index -> Bits
epsilonRating xs index
  | length xs == 1 = head xs
  | otherwise = epsilonRating (filter (\ys -> ys !! index /= mostCommonBits xs !! index) xs) (index + 1)

binToDec :: Bits -> Int
binToDec xs = fst $ foldr (\x (acc, i) -> (acc + x * (2 ^ i), i + 1)) (0, 0) xs

rund3 :: IO ()
rund3 = do
  contents <- readFile "./data/dayThree.txt"
  let bitsList = map (map digitToInt) $ lines contents
  let gammaRate = binToDec $ mostCommonBits bitsList
  let epsilonRate = binToDec $ map (\x -> if x == 0 then 1 else 0) $ mostCommonBits bitsList
  let o2SRatingDec = binToDec $ o2SRating bitsList 0
  let epsilsonRatingDec = binToDec $ epsilonRating bitsList 0
  print $ gammaRate * epsilonRate
  print $ epsilsonRatingDec * o2SRatingDec
