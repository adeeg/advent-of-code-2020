import System.IO

checkLocs :: [String] -> Int -> [Bool]
checkLocs []     _ = []
checkLocs (x:xs) n = (x !! n == '#') : checkLocs xs (n + 3)

main = do
    text        <- readFile "input.txt"
    let input = lines text

    let makeInfinite  = map cycle input
    let collisions    = checkLocs makeInfinite 0
    let numCollisions = sum $ map fromEnum collisions

    putStr (show numCollisions ++ "\n")