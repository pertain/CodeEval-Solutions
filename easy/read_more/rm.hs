{- rd.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Read More
 - https://www.codeeval.com/open_challenges/167/
 -
 - =================================================
 - THIS WAS SUBMITTED BUT IS PARTIALLY CORRECT (90%)
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
    | len > 55  = rdMr
    | otherwise = s
    where
        len     = length s
        trunc   = head $ splitPlaces [40] s
        splt    = words trunc
        lenSplt = length splt
        trimmed = unwords $ init splt
        rdMr
            | lenSplt > 1   = trimmed ++ "... <Read More>"
            | otherwise     = trunc ++ "... <Read Less>"

readMoreAll :: [String] -> [String]
readMoreAll = map readMore
