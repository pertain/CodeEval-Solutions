{- td.hs
 -
 - By William Ersing
 -
 -}

import System.Environment (getArgs)
import Data.List.Split (splitPlaces)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ textifyAll $ rmZeroes linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x

stringToInts :: String -> [Int]
stringToInts s = map (bsToInt . LB.pack) (splitPlaces [1,1,1] s)

intGroups :: [String] -> [[Int]]
intGroups = map stringToInts

groupStrings :: String -> [String]
groupStrings s
    | length s > 9 || null s = error "Invalid Integer"
    | length s == 9 = splitPlaces [3,3,3] s
    | length s == 8 = splitPlaces [2,3,3] s
    | length s == 7 = splitPlaces [1,3,3] s
    | length s == 6 = splitPlaces [3,3] s
    | length s == 5 = splitPlaces [2,3] s
    | length s == 4 = splitPlaces [1,3] s
    | not (null s)  = splitPlaces [3] s

padLists :: [[Int]] -> [[Int]]
padLists iss
    | length iss == 3   = iss
    | length iss == 2   = [] : iss
    | length iss == 1   = [[],[]] ++ iss
    | null iss          = [[],[],[]]

padZeroes :: [Int] -> [Int]
padZeroes is
    | length is == 3    = is
    | length is == 2    = 0 : is
    | length is == 1    = [0,0] ++ is
    | null is           = [0,0,0]

padAll :: [[Int]] -> [[Int]]
padAll lss = map padZeroes (padLists lss)

parseMillions :: [Int] -> String
parseMillions mills @(h:t:o:_)
    | mills == [0,0,0] = ""
    | t /= 1 = printHundreds h ++ printTens t o ++ printOnes o ++ "Million"
    | t == 1 = printHundreds h ++ printTens t o ++ "Million"

parseThousands :: [Int] -> String
parseThousands thous @(h:t:o:_)
    | thous == [0,0,0] = ""
    | t /= 1 = printHundreds h ++ printTens t o ++ printOnes o ++ "Thousand"
    | t == 1 = printHundreds h ++ printTens t o ++ "Thousand"

parseHundreds :: [Int] -> String
parseHundreds hunds @(h:t:o:_)
    | hunds == [0,0,0] = ""
    | t /= 1 = printHundreds h ++ printTens t o ++ printOnes o
    | t == 1 = printHundreds h ++ printTens t o

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

-- t is the second digit of two (i.e. for 13 >> t = 3)
printTeens :: Int -> String
printTeens o
    | o == 0 = "Ten"
    | o == 1 = "Eleven"
    | o == 2 = "Twelve"
    | o == 3 = "Thirteen"
    | o == 4 = "Fourteen"
    | o == 5 = "Fifteen"
    | o == 6 = "Sixteen"
    | o == 7 = "Seventeen"
    | o == 8 = "Eighteen"
    | o == 9 = "Nineteen"

printTens :: Int -> Int -> String
printTens t o
    | t == 0 = ""
    | t == 1 = printTeens o
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

rmZeroes :: [String] -> [String]
rmZeroes = foldr (\x acc -> if x /= "0" then x : acc else acc) []

prepareInput :: String -> [[Int]]
prepareInput = padAll . intGroups . groupStrings

textify :: [[Int]] -> String
textify lss @(m:t:h:_)= parseMillions m ++ parseThousands t ++ parseHundreds h ++ "Dollars"

textifyAll :: [String] -> [String]
textifyAll = map (textify . prepareInput)
