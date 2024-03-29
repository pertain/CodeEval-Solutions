{- mtl.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 -
 - (MODERATE) Mth to Last Element
 - https://www.codeeval.com/open_challenges/10/
 -
 - ====================================================
 -
 - It reads in a file where each line contains a
 - space-separated list of characters, then an integer.
 -
 - Example:     a b c d 4
 -
 - The terminating integer represents the index of the
 - character to be extracted from the end of the list
 - (mth from last element (1-base)).
 -
 - From the example above:
 -
 -  index:  4
 -  list:   a b c d
 -  
 -  mthFromLast: a
 -
 - ====================================================
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ mthList linesList

-- convert ByteString to Int (if valid)
bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x

-- parses out the given index (last element in each string)
lineIndex :: String -> Int
lineIndex = bsToInt . LB.pack . last . words

-- yields the given list sans index (everything except last element)
sansIndex :: String -> [String]
sansIndex s = init $ words s

-- determine if the given index is valid for the given list
validIndex :: String -> Bool
validIndex s
    | x > 0 && x <= length (sansIndex s)    = True
    | otherwise                             = False
    where
        x = lineIndex s

-- yields mth to last element in a given list
mthFromLast :: String -> String
mthFromLast s = reverse (sansIndex s) !! (lineIndex s - 1)

-- yields list of mthFromLast elements
mthList :: [String] -> [String]
mthList = foldr (\x acc -> if validIndex x then mthFromLast x : acc else acc) []
