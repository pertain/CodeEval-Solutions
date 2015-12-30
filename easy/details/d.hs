{- d.hs
 -
 - By William Ersing
 -
 - CodeEval Challenge (EASY) Details
 - https://www.codeeval.com/open_challenges/183/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ detailsAll linesList

splitLine :: String -> [String]
splitLine = splitOn ","

dotsBetween :: [String] -> [Int]
dotsBetween = map (length . takeWhile (== '.') . dropWhile (== 'X'))

fewestDots :: [Int] -> Int
fewestDots = minimum

detailsLine :: String -> Int
detailsLine = fewestDots . dotsBetween . splitLine

detailsAll :: [String] -> [String]
detailsAll = map (show . detailsLine)
