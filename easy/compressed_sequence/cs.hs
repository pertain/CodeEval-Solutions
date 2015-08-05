{- cs.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Compressed Sequence
 - https://www.codeeval.com/open_challenges/128/
 -}

import System.Environment (getArgs)
import Data.List

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ numCountsAll linesList

numberGroups :: String -> [[String]]
numberGroups = group . words

numCountsLine :: [[String]] -> String
numCountsLine = tail . foldr (\x acc -> (' ' : show (length x) ++ (' ' : head x)) ++ acc) ""

numCountsAll :: [String] -> [String]
numCountsAll = map (numCountsLine . numberGroups)
