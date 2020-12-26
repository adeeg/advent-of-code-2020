import System.IO
import Data.List.Split
import Data.Map (Map, fromList, (!), member)
import Data.Char

tuplify :: [a] -> (a, a)
tuplify xs = (head xs, xs !! 1)

lastN :: [a] -> Int -> [a]
lastN xs n = drop (length xs - n) xs

checkPassportValid :: Map String String -> Bool
checkPassportValid m = all (`member` m) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
                    && checkByrValid byr 1920 2002
                    && checkByrValid iyr 2010 2020
                    && checkByrValid eyr 2020 2030
                    && checkHgtValid hgt
                    && checkEclValid ecl
                    && checkHclValid hcl
                    -- leading zeros?
                    && checkDigitLength pid 9
    where byr = m ! "byr"
          iyr = m ! "iyr"
          eyr = m ! "eyr"
          hgt = m ! "hgt"
          ecl = m ! "ecl"
          hcl = m ! "hcl"
          pid = m ! "pid"

checkDigitLength :: String -> Int -> Bool 
checkDigitLength s n = length s == n && all isDigit s

checkByrValid :: String -> Int -> Int -> Bool
checkByrValid s min max = checkDigitLength s 4 && x >= min && x <= max
    where x = read s :: Int

checkEclValid :: String -> Bool
checkEclValid s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

checkHgtValid :: String -> Bool
checkHgtValid s = case unit of
        "cm" -> all isDigit numS && num >= 150 && num <= 193
        "in" -> all isDigit numS && num >=  59 && num <= 76
        _   -> False 
    where unit = lastN s 2
          numS  = take (length s - 2) s
          num   = read numS :: Int

checkHclValid :: String -> Bool
checkHclValid s = head s == '#' && length (tail s) == 6 && all (\x -> isDigit x || x `elem` ['a'..'f']) (tail s)

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