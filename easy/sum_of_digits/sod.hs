{- sod.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Sum of Digits
 - https://www.codeeval.com/open_challenges/21/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitEvery)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ lineSums linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

sumLine :: String -> Int
sumLine s = sum $ map (bsToInt . LB.pack) (splitEvery 1 s)

lineSums :: [String] -> [String]
lineSums = map (show . sumLine)
