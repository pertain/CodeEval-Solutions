{- rd.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Read More
 - https://www.codeeval.com/open_challenges/167/
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
        trunc       = head $ splitPlaces [40] s
        truncLast   = last trunc
        initTrunc   = init trunc
        splitTrunc  = words trunc
        trimSplit   = unwords $ init splitTrunc
        rdMr
            | length splitTrunc == 1    &&  truncLast == ' '    = initTrunc ++ "... <Read More>"
            | length splitTrunc > 1     &&  truncLast == ' '    = initTrunc ++ "... <Read More>"
            | length splitTrunc > 1     &&  truncLast /= ' '    = trimSplit ++ "... <Read More>"
            | otherwise                                         = trunc ++ "... <Read More>"

readMoreAll :: [String] -> [String]
readMoreAll = map readMore
