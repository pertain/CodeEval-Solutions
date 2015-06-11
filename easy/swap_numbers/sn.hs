{- sn.hs
 -
 - By William Ersing
 -
 - This is a CodeEval challenge
 -
 - (EASY) Swap Numbers
 - https://www.codeeval.com/open_challenges/196/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitPlaces, splitOn)

splitPrefixSuffix :: String -> [String]
splitPrefixSuffix s = splitPlaces [1,length s - 2,1] s

swapPrefixSuffix :: [String] -> String
swapPrefixSuffix []         = []
swapPrefixSuffix [x]        = x
swapPrefixSuffix [x,y,z]    = z ++ y ++ x

swapOneLine :: String -> String
swapOneLine s = unwords $ map (swapPrefixSuffix . splitPrefixSuffix) (splitOn " " s)

swapAllLines :: [String] -> [String]
swapAllLines = map swapOneLine

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ swapAllLines linesList
