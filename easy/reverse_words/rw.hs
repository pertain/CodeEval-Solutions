{- rw.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Reverse Words
 - https://www.codeeval.com/open_challenges/8/
 -}

import System.Environment (getArgs)
import Data.List (null)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ revAllLists linesList

revList :: String -> String
revList = unwords . reverse . words

revAllLists :: [String] -> [String]
revAllLists = foldr (\x acc -> if null x then acc else revList x : acc) []
