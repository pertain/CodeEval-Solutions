{- htd.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Hex to Decimal
 - https://www.codeeval.com/open_challenges/67/
 -}

import System.Environment (getArgs)
import Data.Char (toLower)

main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ decimalizeAll linesList

hexToInt :: Char -> Int
hexToInt chr
    | c == '0'  = 0
    | c == '1'  = 1
    | c == '2'  = 2
    | c == '3'  = 3
    | c == '4'  = 4
    | c == '5'  = 5
    | c == '6'  = 6
    | c == '7'  = 7
    | c == '8'  = 8
    | c == '9'  = 9
    | c == 'a'  = 10
    | c == 'b'  = 11
    | c == 'c'  = 12
    | c == 'd'  = 13
    | c == 'e'  = 14
    | c == 'f'  = 15
    | otherwise = error "Bad hex char (hexToInt)"
    where
        c = toLower chr

hexToDec :: String -> Int
hexToDec s = sum $ map (uncurry (*)) posValPairs
    where
        hexVals     = foldl (\acc x -> hexToInt x : acc) [] s
        posValPairs = zip hexVals (map (16^) [0,1..])

decimalizeAll :: [String] -> [String]
decimalizeAll = map (show . hexToDec)
