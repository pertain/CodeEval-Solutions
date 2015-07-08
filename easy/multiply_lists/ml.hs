{- ml.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Multiply Lines
 - https://www.codeeval.com/open_challenges/113/
 -
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ parseAllLines linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

strsToInts :: [String] -> [Int]
strsToInts = map (bsToInt . LB.pack)

splitLn :: String -> [[Int]]
splitLn s = map strsToInts strLsts
    where
        strLsts = map words (splitOn " | " s)

multiplyLn :: [[Int]] -> [Int]
multiplyLn is = zipWith (*) (head is) (last is)

parseLn :: String -> String
parseLn is = unwords strs
    where
        strs = map show (multiplyLn $ splitLn is)

parseAllLines :: [String] -> [String]
parseAllLines = map parseLn
