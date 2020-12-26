import System.IO
import Text.Parsec
import Text.Parsec.String

-- min, max, char
data Policy = Policy Int Int Char
    deriving (Show)

number :: Parser Int
number = read <$> many1 digit

word :: Parser String
word = many1 letter

parsePolicy :: Parser Policy
parsePolicy = Policy <$> number <* char '-' <*> number <* space <*> letter

parsePolyPwd :: Parser (Policy, String)
parsePolyPwd = (,) <$> parsePolicy <* char ':' <* space <*> word

parseLine :: String -> (Policy, String)
parseLine s = case parse parsePolyPwd "" s of
                Left  err -> (Policy 0 0 '_', "dog")
                Right ok  -> ok

checkPolicyWordValid :: (Policy, String) -> Bool
checkPolicyWordValid (Policy min max f, s) = pos1Valid /= pos2Valid
    where pos1Valid = s !! (min - 1) == f
          pos2Valid = s !! (max - 1) == f

main = do
    text        <- readFile "input.txt"
    let input = lines text

    let policyPairs = map parseLine input
    let validities  = map checkPolicyWordValid policyPairs
    let numValid    = sum $ map fromEnum validities

    putStr (show numValid ++ "\n")