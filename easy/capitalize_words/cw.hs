{- cw.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Capitalize Words
 - https://www.codeeval.com/open_challenges/93/
 -}

import System.Environment (getArgs)
import Data.Char

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ capAllLines linesList

capWord :: String -> String
capWord s = toUpper (head s) : tail s

capSentence :: String -> String
capSentence = unwords . map capWord . words

capAllLines :: [String] -> [String]
capAllLines = map capSentence
