{- td.hs
 -
 - By William Ersing
 -
 -}

import System.Environment (getArgs)
import Data.List.Split (chunksOf, splitPlaces)
import qualified Data.ByteString.Lazy.Char8 as LB

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x

-- convert string of numeric Chars to list of individual Ints
stringToInts :: String -> [Int]
stringToInts str = map (bsToInt . LB.pack) (chunksOf 1 str)

-- this was just written -- this output should be fed into strings to ints (or do it here)
-- this allows me to have three helper functions:
--      ~ Millions >> first item in list
--          > always prints "Million"
--          > passes to printOnes if only one digit
--          > passes to printTens (and printOnes vicariously) if 2 digits
--          > passes to printHundreds (and printTens and printOnes) if 3 digits
--      ~ Thousands >> second item in list
--          > always prints "Thousand"
--          > all other steps from above are repeated here
--      ~ Hundreds >> third item in list
--          > always prints "Hundred"
--          > all other steps from above are repeated here
-- continue reworking/modifying print functions below to conform to the
-- approach listed above
groupNums s
    | length s == 9 = splitPlaces [3,3,3] s
    | length s == 8 = splitPlaces [2,3,3] s
    | length s == 7 = splitPlaces [1,3,3] s
    | length s == 6 = splitPlaces [3,3] s
    | length s == 5 = splitPlaces [2,3] s
    | length s == 4 = splitPlaces [1,3] s
    | length s == 3 = splitPlaces [3] s
    | length s <= 2 = chunksOf 3 s

printGenSize :: [Int] -> String
printGenSize str
    | length str > 9 || null str    = "Invalid"
    | length str > 6                = "Million"
    | length str > 3                = "Thousand"
    | length str > 2                = "Hundred"
    | length str > 1                = "Teens"
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
    | s == 20 = "Twenty"
    | s > 20 && s < 30 = "Twenty" ++ printOnes (bsToInt $ LB.pack $ tail $ show s)
    | s == 30 = "Thirty"
    | s == 40 = "Forty"
    | s == 50 = "Fifty"
    | s == 60 = "Sixty"
    | s == 70 = "Seventy"
    | s == 80 = "Eighty"
    | s == 90 = "Ninety"

