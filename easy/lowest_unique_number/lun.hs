{- lun.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 -
 - (EASY) Lowest Unique Number
 - https://www.codeeval.com/open_challenges/103/
 -
 - ========================================================
 -
 - It reads in a file where each line contains N integers
 - from 1 to 9. Each line of integers represents a game
 - with N players. In this game every player writes down
 - a number from 1 to 9 on a piece of paper. When all
 - players have submitted a number, then they are revealed.
 - The winner is the player that has submitted the lowest,
 - unique number (LUN).
 -
 - For each line in the input file, the program prints the
 - index of LUN (the winner). If there are is no winner,
 - the output is 0.
 -
 - Examples:
 -
 -      3 3 9 1 6 5 8 1 5 3
 -
 -          Uniques                 > 6 8 9
 -          Lowest Unique (LUN)     > 6
 -          Index of LUN (Winner)   > 5
 -
 -
 -      9 2 9 9 1 8 8 8 2 1 1
 -
 -          Uniques                 > n/a
 -          Lowest Unique (LUN)     > n/a
 -          Index of LUN (Winner)   > 0
 -
 - ========================================================
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.List as DL

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ print $ getWinnerList linesList

-- convert ByteString to Int (if valid)
bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x

-- convert string (of int chars) to list of Ints
stringToInts :: String -> [Int]
stringToInts = map (bsToInt . LB.pack) . words

-- yields the index of the lowest unique number
indexOfLUN :: Int -> [Int] -> Int
indexOfLUN lun lst = case DL.elemIndex lun lst of
    Nothing     -> 0
    Just x      -> x + 1

-- strips all elements that appear more than once (only retains uniques)
getUniques :: [Int] -> [Int]
getUniques = map head . filter (\l -> length l == 1) . DL.group . DL.sort

-- yields the lowest unique number in the list
getLUN :: [Int] -> Int
getLUN lst
    | null u    = 0
    | otherwise = minimum u
    where
        u = getUniques lst

-- yields the winning player (i.e. index of LUN) for one list
getWinner :: [Int] -> Int
getWinner lst   = indexOfLUN (getLUN lst) lst

-- yields a list of winning players
getWinnerList :: [String] -> [Int]
getWinnerList = map (getWinner . stringToInts)
