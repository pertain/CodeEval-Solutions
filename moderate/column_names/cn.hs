{- cn.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (Moderate): Column Names
 - https://www.codeeval.com/open_challenges/197/
 -}

import System.Environment
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ getAllColumns linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x

stringToInt :: String -> Int
stringToInt = bsToInt . LB.pack

oneLetter :: Int -> String
oneLetter i
    | i == 0    = ""
    | i == 1    = "A"
    | i == 2    = "B"
    | i == 3    = "C"
    | i == 4    = "D"
    | i == 5    = "E"
    | i == 6    = "F"
    | i == 7    = "G"
    | i == 8    = "H"
    | i == 9    = "I"
    | i == 10   = "J"
    | i == 11   = "K"
    | i == 12   = "L"
    | i == 13   = "M"
    | i == 14   = "N"
    | i == 15   = "O"
    | i == 16   = "P"
    | i == 17   = "Q"
    | i == 18   = "R"
    | i == 19   = "S"
    | i == 20   = "T"
    | i == 21   = "U"
    | i == 22   = "V"
    | i == 23   = "W"
    | i == 24   = "X"
    | i == 25   = "Y"
    | i == 26   = "Z"
    | otherwise = error "Invalid Integer"

twoLetters :: Int -> String
twoLetters i
    | i < 53    = 'A' : oneLetter (i - 26)
    | i < 79    = 'B' : oneLetter (i - 52)
    | i < 105   = 'C' : oneLetter (i - 78)
    | i < 131   = 'D' : oneLetter (i - 104)
    | i < 157   = 'E' : oneLetter (i - 130)
    | i < 183   = 'F' : oneLetter (i - 156)
    | i < 209   = 'G' : oneLetter (i - 182)
    | i < 235   = 'H' : oneLetter (i - 208)
    | i < 261   = 'I' : oneLetter (i - 234)
    | i < 287   = 'J' : oneLetter (i - 260)
    | i < 313   = 'K' : oneLetter (i - 286)
    | i < 339   = 'L' : oneLetter (i - 312)
    | i < 365   = 'M' : oneLetter (i - 338)
    | i < 391   = 'N' : oneLetter (i - 364)
    | i < 417   = 'O' : oneLetter (i - 390)
    | i < 443   = 'P' : oneLetter (i - 416)
    | i < 469   = 'Q' : oneLetter (i - 442)
    | i < 495   = 'R' : oneLetter (i - 468)
    | i < 521   = 'S' : oneLetter (i - 494)
    | i < 547   = 'T' : oneLetter (i - 520)
    | i < 573   = 'U' : oneLetter (i - 546)
    | i < 599   = 'V' : oneLetter (i - 572)
    | i < 625   = 'W' : oneLetter (i - 598)
    | i < 651   = 'X' : oneLetter (i - 624)
    | i < 677   = 'Y' : oneLetter (i - 650)
    | i < 703   = 'Z' : oneLetter (i - 676)
    | otherwise = error "Invalid Integer"

threeLetters :: Int -> String
threeLetters i
    | i < 1379  = 'A' : twoLetters (26 + i - 702)
    | i < 2055  = 'B' : twoLetters (26 + i - 1378)
    | i < 2731  = 'C' : twoLetters (26 + i - 2054)
    | i < 3407  = 'D' : twoLetters (26 + i - 2730)
    | i < 4083  = 'E' : twoLetters (26 + i - 3406)
    | i < 4759  = 'F' : twoLetters (26 + i - 4082)
    | i < 5435  = 'G' : twoLetters (26 + i - 4758)
    | i < 6111  = 'H' : twoLetters (26 + i - 5434)
    | i < 6787  = 'I' : twoLetters (26 + i - 6110)
    | i < 7463  = 'J' : twoLetters (26 + i - 6786)
    | i < 8139  = 'K' : twoLetters (26 + i - 7462)
    | i < 8815  = 'L' : twoLetters (26 + i - 8138)
    | i < 9491  = 'M' : twoLetters (26 + i - 8814)
    | i < 10167 = 'N' : twoLetters (26 + i - 9490)
    | i < 10843 = 'O' : twoLetters (26 + i - 10166)
    | i < 11519 = 'P' : twoLetters (26 + i - 10842)
    | i < 12195 = 'Q' : twoLetters (26 + i - 11518)
    | i < 12871 = 'R' : twoLetters (26 + i - 12194)
    | i < 13547 = 'S' : twoLetters (26 + i - 12870)
    | i < 14223 = 'T' : twoLetters (26 + i - 13546)
    | i < 14899 = 'U' : twoLetters (26 + i - 14222)
    | i < 15575 = 'V' : twoLetters (26 + i - 14898)
    | i < 16251 = 'W' : twoLetters (26 + i - 15574)
    | i < 16927 = 'X' : twoLetters (26 + i - 16250)
    | i < 17603 = 'Y' : twoLetters (26 + i - 16926)
    | i < 18279 = 'Z' : twoLetters (26 + i - 17602)
    | otherwise = error "Invalid Integer"

getColumnHeader :: Int -> String
getColumnHeader i
    | i == 0    = ""
    | i < 27    = oneLetter i
    | i < 703   = twoLetters i
    | i < 18279 = threeLetters i
    | otherwise = error "Invalid Integer"

getAllColumns :: [String] -> [String]
getAllColumns = map (getColumnHeader . stringToInt)
