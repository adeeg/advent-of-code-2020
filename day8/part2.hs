import System.IO
import Text.Parsec
import Text.Parsec.String

number :: Parser Int
number = read <$> many1 digit

data Instr = Acc Int
           | Jmp Int
           | Nop Int
    deriving (Show)

parseNumber :: Parser Int
parseNumber = char '+' *> number
          <|> char '-' *> pure negate <*> number

parseInstr :: Parser Instr
parseInstr = Acc <$ string "acc " <*> parseNumber
         <|> Jmp <$ string "jmp " <*> parseNumber
         <|> Nop <$ string "nop " <*> parseNumber

parseLine :: String -> Instr
parseLine s = case parse parseInstr "" s of
                Left  err -> Nop 0
                Right ok  -> ok

-- instrs, marked, idx, acc
evalInstr :: [Instr] -> [Int] -> Int -> Int
evalInstr xs m n
    | n >= length xs = 0
    | s              =
        -- repair
        case i of (Acc x) -> x + evalInstr xs (n:m) (n + 1)
                  (Jmp x) -> evalInstr xs (n:m) (n + 1)
                  (Nop x) -> evalInstr xs (n:m) (n + x)
    | otherwise      =
        case i of (Acc x) -> x + evalInstr xs (n:m) (n + 1)
                  (Jmp x) -> evalInstr xs (n:m) (n + x)
                  (Nop x) -> evalInstr xs (n:m) (n + 1)
    where s = n `elem` m
          i = xs !! n

-- instrs, marked, idx, acc
traceInstr :: [Instr] -> [Int] -> Int -> [Int]
traceInstr xs m n
    | n >= length xs = [n*100]
    | s              =
        -- repair
        case i of (Acc x) -> n : traceInstr xs (n:m) (n + 1)
                  (Jmp x) -> n : traceInstr xs (n:m) (n + 1)
                  (Nop x) -> n : traceInstr xs (n:m) (n + x)
    | otherwise      =
        case i of (Acc x) -> n : traceInstr xs (n:m) (n + 1)
                  (Jmp x) -> n : if (n + x) `elem` m then traceInstr xs (n:m) (n + 1) else traceInstr xs (n:m) (n + x)
                  (Nop x) -> n : if (n + 1) `elem` m then traceInstr xs (n:m) (n + x) else traceInstr xs (n:m) (n + 1) 
    where s = n `elem` m
          i = xs !! n

genAltInstrs :: [Instr] -> [Instr]
genAltInstrs (x:xs) = x : xs


main = do
    text     <- readFile "input.txt"
    let input = lines text

    -- parse into datatype
    let instrs = map parseLine input
    -- generate all alternative instrs

    -- evaluate
    -- let e = evalInstr instrs [] 0
    let e = traceInstr instrs [] 0

    putStr (show e ++ "\n")