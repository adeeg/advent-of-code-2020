import System.IO
import Text.Parsec
import Text.Parsec.String

number :: Parser Int
number = read <$> many1 digit

data Instr = Acc Int
           | Jmp Int
           | Nop
    deriving (Show)

parseNumber :: Parser Int
parseNumber = char '+' *> number
          <|> char '-' *> pure negate <*> number

parseInstr :: Parser Instr
parseInstr = Acc <$ string "acc " <*> parseNumber
         <|> Jmp <$ string "jmp " <*> parseNumber
         <|> Nop <$ string "nop"

parseLine :: String -> Instr
parseLine s = case parse parseInstr "" s of
                Left  err -> Nop
                Right ok  -> ok

-- instrs, marked, idx, acc
evalInstr :: [Instr] -> [Int] -> Int -> Int
evalInstr xs m n = if s then 0 else case i of
        (Acc x) -> x + evalInstr xs (n:m) (n + 1)
        (Jmp x) -> evalInstr xs (n:m) (n + x)
        Nop     -> evalInstr xs (n:m) (n + 1)
    where s = n `elem` m
          i = xs !! n

main = do
    text     <- readFile "input.txt"
    let input = lines text

    -- parse into datatype
    let instrs = map parseLine input
    -- evaluate
    let e = evalInstr instrs [] 0

    putStr (show e ++ "\n")