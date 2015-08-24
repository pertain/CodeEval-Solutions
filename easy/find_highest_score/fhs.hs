{- fhs.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Find the Highest Score
 - https://www.codeeval.com/open_challenges/208/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (transpose)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ maxScoresAll linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

strsToInts :: [String] -> [Int]
strsToInts = map (bsToInt . LB.pack)

maxScoresLine :: String -> String
maxScoresLine s = unwords maxScoreInts
    where
        splitLn         = splitOn "|" s
        columnInts      = map (strsToInts . words) splitLn
        categoryScores  = transpose columnInts
        maxScoreInts    = map (show . maximum) categoryScores

maxScoresAll :: [String] -> [String]
maxScoresAll = map maxScoresLine
