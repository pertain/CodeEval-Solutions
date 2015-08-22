{- rmc.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Rightmost Char
 - https://www.codeeval.com/open_challenges/31/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ rightmostIndices linesList

rightmostIndex :: String -> Int
rightmostIndex s = length upToTarget - 1
    where
        msg             = takeWhile (','/=) s
        target          = last s
        upToTarget      = dropWhile (target/=) (reverse msg)

rightmostIndices :: [String] -> [String]
rightmostIndices = map (show . rightmostIndex)
