{- ll.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 -
 - (MODERATE) Longest Lines
 - https://www.codeeval.com/open_challenges/2/
 -
 - =====================================================
 -
 - It reads in a file where the first line contains
 - a valid positive integer (N), and every other line
 - contains a string of varying length. The task is
 - to print out the N longest lines in descending order.
 -
 - Example:
 -
 -      Input:  2
 -              Hello World
 -              CodeEval
 -              Quick Fox
 -              A
 -              San Francisco
 -
 -                  >>  N = 2
 -
 -      Output: San Francisco
 -              Hello World
 -
 - =====================================================
 -}

import System.Environment (getArgs)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    let takeSize = bsToInt $ LB.pack $ head linesList
    let stringList = tail linesList
    mapM_ putStrLn $ strListDesc stringList takeSize

-- convert ByteString to Int (if valid)
bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing -> error "Not an Int"
    Just (x,_) -> x

-- yields first element in a 3-tuple
getFst :: (a,b,c) -> a
getFst (a,_,_) = a

-- yields second element in a 3-tuple
getSnd :: (a,b,c) -> b
getSnd (_,b,_) = b

-- yields third element in a 3-tuple
getThd :: (a,b,c) -> c
getThd (_,_,c) = c

-- yields index of element as Int (from Maybe Int)
getIndex :: String -> [String] -> Int
getIndex elm lst = fromMaybe (error "element not found") (elemIndex elm lst)

-- yields list of tuples where (fst, snd) = (element index, element length)
tupString :: [String] -> [(Int, String, Int)]
tupString lst = foldr (\x acc -> (getIndex x lst, x, length x) : acc) [] lst

-- sorts tuple list by element length (snd)
sortByLongest :: [(Int, String, Int)] -> [(Int, String, Int)]
sortByLongest = sortBy (flip $ comparing getThd)

-- yields sorted (descending) list of the n longest strings
strListDesc :: [String] -> Int -> [String]
strListDesc lst n = take n $ map getSnd (sortByLongest $ tupString lst)
