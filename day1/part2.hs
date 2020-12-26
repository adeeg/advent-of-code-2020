import System.IO
import Data.Map (Map, fromList, (!), member)
import Data.List
import Data.Maybe

thd :: (a, b, c) -> c
thd (_, _, z) = z

tprod :: (Int, Int, Int) -> Int
tprod (x, y, z) = x * y * z

bruteFind :: [((Int, Int), Int, Int)] -> [Int] -> Maybe (Int, Int, Int)
bruteFind []                  _    = Nothing 
bruteFind (((x, y), s, f):xs) nums = if isNothing found then bruteFind xs nums else Just (x, y, fromJust found)
    where found = find (== f) nums

main = do
    text        <- readFile "input.txt"
    let textInts = map read (lines text) :: [Int]

    -- https://stackoverflow.com/questions/34044366/how-to-extract-all-unique-pairs-of-a-list-in-haskell
    let xxx      = [ (x, y) | (x:ys) <- tails textInts, y <- ys ]
    -- (pair, sum, 2020 - sum)
    let fmtd     = map (\x -> (x, uncurry (+) x, 2020 - uncurry (+) x)) xxx
    let fmtdF    = filter (\x -> thd x > 0) fmtd
    let find     = bruteFind fmtdF textInts
    let ans      = if isNothing find then Nothing else Just (tprod (fromJust find))

    putStr (show ans ++ "\n")