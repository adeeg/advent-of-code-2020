import System.IO
import Data.List.Split
import Data.List

main = do
    text     <- readFile "input.txt"
    let input = lines text

    -- nub removes duplicates
    let groups  = map (nub . concat) $ splitOn [""] input
    let ans     = sum $ map length groups

    putStr (show ans ++ "\n")