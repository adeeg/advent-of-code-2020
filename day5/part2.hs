import System.IO
import Data.List

lowerHalf ::(Integer, Integer) -> (Integer, Integer)
lowerHalf (min, max) = (min, max - ceiling (fromInteger (max - min) / 2))

upperHalf ::(Integer, Integer) -> (Integer, Integer)
upperHalf (min, max) = (min + ceiling (fromInteger (max - min) / 2), max)

parseLetter :: Char -> (Integer, Integer) -> (Integer, Integer)
parseLetter 'F' = lowerHalf
parseLetter 'L' = lowerHalf
parseLetter 'B' = upperHalf
parseLetter 'R' = upperHalf
parseLetter _   = id

parseWord :: String -> (Integer, Integer) -> (Integer, Integer)
parseWord s (x, y) = foldl (flip parseLetter) (x, y) s

parseLine :: String -> (Integer, Integer)
parseLine s = (row, col)
    where row = fst $ parseWord (take 7 s) (0, 127)
          col = fst $ parseWord (drop 7 s) (0,   7)

calcSeatID :: (Integer, Integer) -> Integer
calcSeatID (r, c) = r * 8 + c

main = do
    text     <- readFile "input.txt"
    let input = lines text

    let rowsCols = map parseLine input
    let seatIDs  = map calcSeatID rowsCols
    let ans      = head $ [(minimum seatIDs) .. (maximum seatIDs)] \\ seatIDs

    putStr (show ans ++ "\n")