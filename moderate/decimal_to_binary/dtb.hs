{- dtb.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (MODERATE) Decimal to Binary
 - https://www.codeeval.com/open_challenges/27/
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ binarizeAll linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

strToInt :: String -> Int
strToInt = bsToInt . LB.pack

decToBin :: Int -> [Int]
decToBin i
    | i == 0    = []
    | otherwise = rem i 2 : decToBin (div i 2)

binarizeLine :: String -> String
binarizeLine s
    | s == "0"  = s
    | otherwise = reverse . concatMap show . decToBin $ strToInt s

binarizeAll :: [String] -> [String]
binarizeAll = map binarizeLine
