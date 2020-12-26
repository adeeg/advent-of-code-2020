import System.IO

-- (Int, Int) is our slope (changeX, changeY) & position (posX, posY)
checkLocs :: [String] -> (Int, Int) -> (Int, Int) -> [Bool]
checkLocs ms (dx, dy) (x, y) = if y >= length ms then [] else exists
    where exists = (ms !! y !! x == '#') : checkLocs ms (dx, dy) (x + dx, y + dy)

main = do
    text        <- readFile "input.txt"
    let input = lines text

    let makeInfinite  = map cycle input

    let slopes        = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    let collisions    = map (\x -> checkLocs makeInfinite x (0, 0)) slopes
    let numCollisions = map (sum . map fromEnum) collisions
    let ans           = product numCollisions

    putStr (show ans ++ "\n")