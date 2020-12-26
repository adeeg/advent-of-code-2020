import System.IO
import Data.Map (Map, fromList, (!), member)

listToMap :: [Int] -> Map Int Int
listToMap = fromList . Prelude.map (\x -> (x, 2020 - x))

-- searches map for something where (k1: _) for some (v1: k1)
-- returns (v1, k1)
searchMapMatch :: [Int] -> Map Int Int -> (Int, Int)
searchMapMatch []     _   = (-1, -1)
searchMapMatch (x:xs) ass = if toFindE then (x, toFind) else searchMapMatch xs ass
    where toFind  = ass ! x
          toFindE = member toFind ass

main = do
    text        <- readFile "input.txt"
    let textInts = map read (lines text) :: [Int]
    let inpMap   = listToMap textInts
    let pair     = searchMapMatch textInts inpMap
    let ans      = uncurry (*) pair
    putStr (show ans ++ "\n")