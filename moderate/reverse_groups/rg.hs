{- rg.hs
 -
 - By William Ersing
 -
 - (MODERATE) Reverse Groups
 - https://www.codeeval.com/open_challenges/71/
 -
 - THIS HAS BEEN SUBMITTED, BUT ONLY RECIEVED PARTIAL CREDIT
 - STILL WORKING TOWARD THE FULL SOLUTION
 -}

import System.Environment (getArgs)
import Data.List (intersperse)
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

validMultiple :: [[String]] -> Int -> Bool
validMultiple s k = length (concat s) `mod` k == 0

revGrps :: [[String]] -> Int -> String
revGrps sss k
    | validMultiple sss k   = concat (concatMap reverse sss)
    | otherwise             = concat (concatMap reverse (init sss) ++ last sss)

finalizeLine :: String -> String
finalizeLine s = intersperse ',' (revGrps grps k)
    where
        splitLn = splitOneOf ",;" s
        k       = strToInt $ last splitLn
        lst     = init splitLn
        grps    = splitEvery k lst

revAllLines :: [String] -> [String]
revAllLines = map finalizeLine
