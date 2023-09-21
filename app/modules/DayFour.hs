module Modules.DayFour (testIt) where

import Data.List.Split (splitOn)

testIt :: String
testIt = "Testing"

-- Input Parsing --
-- Split to lines
-- First line is just an arr
-- Next lines: array of arrays of tuples
main :: IO ()
main = do
  contents <- readFile "./data/day4t.txt"
  let firstLine = splitOn "," $ head $ lines contents
  print firstLine
  putStrLn "Hello, World!"
