module Main where
import DayFour
import DayThree
import DayFive

run :: Int -> IO ()
run day = case day of
    3 -> DayThree.rund3
    4 -> DayFour.rund4
    5 -> DayFive.rund5
    _ -> error "Wrong day"

main :: IO ()
main = do
    putStrLn "Foo"
