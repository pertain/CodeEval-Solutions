{- sc.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Swap Case
 - https://www.codeeval.com/open_challenges/96/
 -}

import System.Environment (getArgs)
import Data.Char (isUpper, toLower, toUpper)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ swapCaseAll linesList

swapCase :: String -> String
swapCase = foldr (\x acc -> if isUpper x then toLower x : acc else toUpper x : acc) []

swapCaseAll :: [String] -> [String]
swapCaseAll = map swapCase
