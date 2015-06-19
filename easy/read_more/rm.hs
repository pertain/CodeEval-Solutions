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
    --putStrLn ""

-- TO DO:   I think the problem comes from the trimTrunc portion.
--          Right now, if the 40 length string ends with a space,
--          then the space is trimmed. But, in doing this, the second
--          part, trimSplit then cuts off another word, when it may
--          only need to trim the last space. Change all this so the
--          (possible) trailing space is only trimmed from strings
--          that contain no spaces (i.e. length splitTrunc == 1), and
--          rather than chopping off an entire word if length splitTrunc > 1,
--          just have it trim the trailing space (if present). If no
--          trailing space is present, then trim the last word as is
--          done now.
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
