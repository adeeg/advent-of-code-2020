import System.IO
import Data.List.Split
import Data.List

main = do
    text     <- readFile "input.txt"
    let input = lines text

    let groups  = splitOn [""] input
    let mutual  = map (foldr1 intersect) groups
    let ans     = sum $ map length mutual

    putStr (show ans ++ "\n")