{- rd.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Read More
 - https://www.codeeval.com/open_challenges/167/
 -
 - =================================================
 - THIS WAS SUBMITTED BUT IS PARTIALLY CORRECT (92%)
 -
 -  ~ must continue to troubleshoot the problem
 - =================================================
 -}

import System.Environment (getArgs)
import Data.List.Split (splitPlaces)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ readMoreAll linesList

readMore :: String -> String
readMore s
    | length s > 55 = rdMr
    | otherwise     = s
    where
        trunc                       = head $ splitPlaces [40] s
        trimTrunc
            | last trunc == ' '     = init trunc
            | otherwise             = trunc
        splitTrunc                  = words trimTrunc
        trimSplit                   = unwords $ init splitTrunc
        rdMr
            | length splitTrunc > 1 = trimSplit ++ "... <Read More>"
            | otherwise             = trimTrunc ++ "... <Read More>"

readMoreAll :: [String] -> [String]
readMoreAll = map readMore
