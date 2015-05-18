{- td.hs
 -
 - By William Ersing
 -
 -}

import System.Environment (getArgs)
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Lazy.Char8 as LB

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x



-- convert string of numeric Chars to list of individual Ints
stringToInts :: String -> [Int]
stringToInts str = map (bsToInt . LB.pack) (chunksOf 1 str)

printGenSize :: [Int] -> String
printGenSize str
    | length str > 9 || null str    = "Invalid"
    | length str > 6                = "Million"
    | length str > 3                = "Thousand"
    | length str > 2                = "Hundred"
    | length str > 1                = "Teen"
    | not (null str)                = printOnes (last str)

printOnes :: Int -> String
printOnes s
    | s == 1 = "One"
    | s == 2 = "Two"
    | s == 3 = "Three"
    | s == 4 = "Four"
    | s == 5 = "Five"
    | s == 6 = "Six"
    | s == 7 = "Seven"
    | s == 8 = "Eight"
    | s == 9 = "Nine"

printTens :: Int -> String
printTens s
    | s == 10 = "Ten"
    | s == 11 = "Eleven"
    | s == 12 = "Twelve"
    | s == 13 = "Thirteen"
    | s == 14 = "Fourteen"
    | s == 15 = "Fifteen"
    | s == 16 = "Sixteen"
    | s == 17 = "Seventeen"
    | s == 18 = "Eighteen"
    | s == 19 = "Nineteen"

