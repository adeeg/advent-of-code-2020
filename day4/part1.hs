import System.IO
import Data.List.Split

import Data.Map (Map, fromList, (!), member)

tuplify :: [a] -> (a, a)
tuplify xs = (head xs, xs !! 1)

checkPassportValid :: Map String String -> Bool
checkPassportValid m = all (`member` m) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main = do
    text        <- readFile "input.txt"
    let input = lines text

    -- parsing
    -- passports into their own lists
    let passports     = splitOn [""] input
    let passportsFull = map concat $ map (map $ wordsBy (== ' ')) passports
    let passportsMap  = map (fromList . map (tuplify . wordsBy (== ':'))) passportsFull

    let validities    = map checkPassportValid passportsMap
    let numValidities = sum $ map fromEnum validities

    putStr (show numValidities ++ "\n")