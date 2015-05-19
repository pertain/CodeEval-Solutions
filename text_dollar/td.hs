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

-- convert string of numeric Chars to list of individual Ints, grouped by decimal place
intGroups :: [String] -> [[Int]]
intGroups ss = map stringToInts chks
    where
        stringToInts    = map (bsToInt . LB.pack)
        chks            = chunksOf 1 ss

-- this was just written -- this output should be fed into stringsToInts (or do it here)
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
groupNums :: String -> [String]
groupNums s
    | length s > 9 || null s = error "Invalid Integer"
    | length s == 9 = splitPlaces [3,3,3] s
    | length s == 8 = splitPlaces [2,3,3] s
    | length s == 7 = splitPlaces [1,3,3] s
    | length s == 6 = splitPlaces [3,3] s
    | length s == 5 = splitPlaces [2,3] s
    | length s == 4 = splitPlaces [1,3] s
    | not (null s)  = splitPlaces [3] s

printGenSize :: [Int] -> String
printGenSize str
    | length str > 3 || null str    = error "Invalid"
    | length str == 3               = "Million"
    | length str == 2               = "Thousand"
    | not (null str)                = printOnes (last str) ++ "Hundred"

--parseMillions :: [Int] -> String

--parseThousands :: [Int] -> String

parseHundreds :: [Int] -> String
parseHundreds h
    | length h == 3 = printHundreds h
    | length h == 2 = printHundreds $ 0 : h
    | length h == 1 = printHundreds $ [0,0] ++ h

printOnes :: Int -> String
printOnes o
    | o == 0 = ""
    | o == 1 = "One"
    | o == 2 = "Two"
    | o == 3 = "Three"
    | o == 4 = "Four"
    | o == 5 = "Five"
    | o == 6 = "Six"
    | o == 7 = "Seven"
    | o == 8 = "Eight"
    | o == 9 = "Nine"

-- s is the second digit of two (i.e. 13 >> 3)
printTeens :: Int -> String
printTeens t
    | t == 0 = "Ten"
    | t == 1 = "Eleven"
    | t == 2 = "Twelve"
    | t == 3 = "Thirteen"
    | t == 4 = "Fourteen"
    | t == 5 = "Fifteen"
    | t == 6 = "Sixteen"
    | t == 7 = "Seventeen"
    | t == 8 = "Eighteen"
    | t == 9 = "Nineteen"

printTens :: Int -> String
printTens t
    | t == 0 = ""
    | t == 1 = ""
    | t == 2 = "Twenty"
    | t == 3 = "Thirty"
    | t == 4 = "Forty"
    | t == 5 = "Fifty"
    | t == 6 = "Sixty"
    | t == 7 = "Seventy"
    | t == 8 = "Eighty"
    | t == 9 = "Ninety"

printHundreds :: Int -> String
printHundreds h
    | h == 0 = ""
    | h >= 0 = printOnes h ++ "Hundred"
