module Day8 (rund8) where

import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Combination = [Character]

type Code = String

type Character = String

type CharToCode = M.Map Character Code

type CodeToChar = M.Map Code Character

data IORow = IORow
  { input :: [Combination],
    output :: [Combination]
  }
  deriving (Show)

correctMapping :: M.Map String String
correctMapping =
  M.fromList
    [ ("abcefg", "0"),
      ("cf", "1"),
      ("acdeg", "2"),
      ("acdfg", "3"),
      ("bcdf", "4"),
      ("abdfg", "5"),
      ("abdefg", "6"),
      ("acf", "7"),
      ("abcdefg", "8"),
      ("abcdfg", "9")
    ]

mapToInt :: [String] -> [String]
mapToInt = map decodeWord
  where
    decodeWord :: String -> String
    decodeWord string = case M.lookup string correctMapping of
      Nothing -> error $ "Missing " ++ string
      Just number -> number

correctCodeMapping :: M.Map Code Character
correctCodeMapping =
  M.fromList $
    map (\(k, v) -> (v, k)) $
      M.toList $
        createCodeMap
          [ ["a", "b", "c", "e", "f", "g"],
            ["c", "f"],
            ["a", "c", "d", "e", "g"],
            ["a", "c", "d", "f", "g"],
            ["b", "c", "d", "f"],
            ["a", "b", "d", "f", "g"],
            ["a", "b", "d", "e", "f", "g"],
            ["a", "c", "f"],
            ["a", "b", "c", "d", "e", "f", "g"],
            ["a", "b", "c", "d", "f", "g"]
          ]

calculateMates :: Character -> [Combination] -> Code
calculateMates char combinations = concat $ sort $ map (show . length) $ filter (elem char) combinations

createLetterCode :: Combination -> [Combination] -> [(Character, Code)]
createLetterCode combination combinations = map (\character -> (character, calculateMates character combinations)) combination

decodeCombinations :: [Combination] -> [[(Character, Code)]]
decodeCombinations combinations = map (`createLetterCode` combinations) combinations

createCodeMap :: [Combination] -> M.Map Character Code
createCodeMap combinations = M.fromList $ concat $ decodeCombinations combinations

convertRow :: CharToCode -> [Combination] -> [String]
convertRow mapping =
  map
    ( concat
        . sort
        . map (mapChar mapping correctCodeMapping)
    )

mapChar :: CharToCode -> CodeToChar -> Character -> Character
mapChar encoding decoding char = case M.lookup char encoding of
  Nothing -> error "No code"
  Just code -> case M.lookup code decoding of
    Just correctChar -> correctChar
    Nothing -> error $ "No char " ++ code

decode :: IORow -> Int
decode ioRow = read $ concat $ mapToInt $ convertRow codeMap (output ioRow)
  where
    codeMap = createCodeMap $ input ioRow

rund8 :: IO ()
rund8 = do
  allInput <- readFile "data/day8.txt"
  let parsedInput = map (map words . splitOn " | ") $ lines allInput
  let splitParsedInput = map (map (map (filter (/= "") . splitOn ""))) parsedInput
  let ioRows = map (\ioRow -> IORow (head ioRow) (last ioRow)) splitParsedInput
  let codeMap = createCodeMap $ input $ head ioRows -- map of current row
  print $ convertRow codeMap (input $ head ioRows)
  print $ sum $ map decode ioRows
