{- nom.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (MODERATE) Number of Ones
 - https://www.codeeval.com/open_challenges/16/
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ onesAll linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

decToBin :: Int -> [Int]
decToBin i
    | i == 0      = []
    | otherwise   = rem i 2 : decToBin (div i 2)

onesLine :: String -> Int
onesLine = sum . decToBin . bsToInt . LB.pack

onesAll :: [String] -> [String]
onesAll = map (show . onesLine)
