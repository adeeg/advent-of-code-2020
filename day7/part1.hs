import System.IO
import Data.Map (Map, fromList, (!), member)

data Bag = Bag String [String] [String]



main = do
    text     <- readFile "input.txt"
    let input = lines text

    let tree = Map String Bag
    -- build tree
    
    -- traverse tree

    putStr (show ans ++ "\n")