{- rg.hs
 -
 - By William Ersing
 -
 - (MODERATE) Reverse Groups
 - https://www.codeeval.com/open_challenges/71/
 -}

import System.Environment (getArgs)
import Data.List (intercalate)
import Data.List.Split (splitOneOf, splitEvery)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ revAllLines linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not and Integer"
    Just (x,_)  -> x

strToInt :: String -> Int
strToInt = bsToInt . LB.pack

-- Determines if list length is a multiple of k
validMultiple :: [[String]] -> Int -> Bool
validMultiple s k
    | k > 0     = length (concat s) `mod` k == 0
    | otherwise = error "Cannot divide by zero"

-- If list length is multiple of k, then reverse every group
-- If it is not multiple of k, then reverse all but last group
revGrps :: [[String]] -> Int -> [String]
revGrps sss k
    | validMultiple sss k   = concatMap reverse sss
    | otherwise             = concatMap reverse (init sss) ++ last sss

-- Split list into groups of size k, reverse them and output as string
finalizeLine :: String -> String
finalizeLine s = intercalate "," (revGrps grps k)
    where
        splitLn = splitOneOf ",;" s
        k       = strToInt $ last splitLn
        lst     = init splitLn
        grps    = splitEvery k lst

revAllLines :: [String] -> [String]
revAllLines = map finalizeLine
