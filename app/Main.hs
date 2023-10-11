module Main where
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8

run :: Int -> IO ()
run day = case day of
    3 -> Day3.rund3
    4 -> Day4.rund4
    5 -> Day5.rund5
    6 -> Day6.rund6
    7 -> Day7.rund7
    8 -> Day8.rund8
    _ -> error "Wrong day"

main :: IO ()
main = do
    putStrLn "Foo"
