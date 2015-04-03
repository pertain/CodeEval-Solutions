{- ibs.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 - It reads in a file where each line contains a
 - space-separated list of numbers to be sorted (L),
 - a delimiter (|) then a number (N) for iterations.
 - The program should print the state of the list
 - after N iterations.
 -
 - Example:
 -
 -  Input:
 -      36 47 78 28 20 79 87 16 8 45 72 69 81 66 60 8 3 86 90 90 | 1
 -
 -          L:  36 47 78 28 20 79 87 16 8 45 72 69 81 66 60 8 3 86 90 90
 -          N:  1
 -
 -  Output:
 -      36 47 28 20 78 79 16 8 45 72 69 81 66 60 8 3 86 87 90 90
 -
 - ---------------------------------------------------------------------------
 - TO DO:
 -  ~ stop sorting when list is sorted
 -      (currently sorts for N (or length L) iterations)
 -  ~ each iteration should sort one fewer element (previously sorted element)
 -      (currently sorts all elements in each iteration)
 - ---------------------------------------------------------------------------
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.List.Split as LS

main :: IO ()
main = do
    inFile <-getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ bubSortList linesList

-- convert ByteString to Int (if valid)
bsToInt :: LB.ByteString -> Int
bsToInt bs =  case LB.readInt bs of
    Nothing -> error "Not an Integer"
    Just (x,_) -> x

-- extract number of iterations (N)
getN :: String -> Int -> Int
getN st sz
    | n > sz    = sz
    | otherwise = n + 1
    where
        n = bsToInt . LB.pack . last $ words st

-- extract list to be sorted (L)
getIntList :: String -> [Int]
getIntList = map (bsToInt . LB.pack) . init . init . words

-- perform one bubble sort iteration
bubSortOnce :: Ord a => [a] -> [a]
bubSortOnce []  = []
bubSortOnce (x:[])  = x:[]
bubSortOnce (x1:x2:xs)
    | x1 > x2   = x2:(bubSortOnce $ x1:xs)
    | otherwise = x1:(bubSortOnce $ x2:xs)

-- perform bubble sort (N iterations) on entire line
bubSortN :: String -> [Int]
bubSortN s = last $ take (getN s $ length sz) $ iterate (bubSortOnce) sz
    where sz = getIntList s

-- convert list of Ints to a string
intsToStrings :: [Int] -> String
intsToStrings = unwords . map show

-- perform bubble sort (N iterations) on every line (i.e. entire file)
bubSortList :: [String] -> [String]
bubSortList = map (intsToStrings . bubSortN)
