{- mc.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Mixed Content
 - https://www.codeeval.com/open_challenges/115/
 -}

import System.Environment
import Data.Char (isNumber, isLetter)
import Data.List (intercalate)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ composeLines linesList

composeNums :: [String] -> String
composeNums = intercalate "," . foldr (\x acc -> if isNumber (head x) then x : acc else acc) []

composeWords :: [String] -> String
composeWords = intercalate "," . foldr (\x acc -> if isLetter (head x) then x : acc else acc) []

handleLineType :: String -> String
handleLineType s
    | all isNumber y    = s
    | all isLetter y    = s
    | otherwise         = composeWords x ++ ('|' : composeNums x)
    where
        x = splitOn "," s
        y = concat x

composeLines :: [String] -> [String]
composeLines = map handleLineType
