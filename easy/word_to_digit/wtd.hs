{- wtd.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Word to Digit
 - https://www.codeeval.com/open_challenges/104/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ intizeFile linesList

wordToInt :: String -> Char
wordToInt s
    | s == "zero"   = '0'
    | s == "one"    = '1'
    | s == "two"    = '2'
    | s == "three"  = '3'
    | s == "four"   = '4'
    | s == "five"   = '5'
    | s == "six"    = '6'
    | s == "seven"  = '7'
    | s == "eight"  = '8'
    | s == "nine"   = '9'

intizeLine :: String -> String
intizeLine s = map wordToInt (splitOn ";" s)

intizeFile :: [String] -> [String]
intizeFile = map intizeLine
