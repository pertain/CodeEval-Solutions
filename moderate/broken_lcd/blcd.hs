{- blcd.hs
 -
 - By William Ersing
 -
 - (MODERATE) Broken LCD
 - https://www.codeeval.com/open_challenges/179/
 -}

import System.Environment (getArgs)
import qualified Data.List.Split as LS

splitLn :: String -> [String]
splitLn = LS.splitOneOf " ;"

getBinaryList :: String -> [String]
getBinaryList = init . splitLn

getTargetNum :: String -> String
getTargetNum = last . splitLn

binaryToDigit :: String -> String
binaryToDigit s
    | s == "11111101"   = "0."
    | s == "01100001"   = "1."
    | s == "11011011"   = "2."
    | s == "11110011"   = "3."
    | s == "01100111"   = "4."
    | s == "10110111"   = "5."
    | s == "10111111"   = "6."
    | s == "11100001"   = "7."
    | s == "11111111"   = "8."
    | s == "11110111"   = "9."
    | s == "11111100"   = "0"
    | s == "01100000"   = "1"
    | s == "11011010"   = "2"
    | s == "11110010"   = "3"
    | s == "01100110"   = "4"
    | s == "10110110"   = "5"
    | s == "10111110"   = "6"
    | s == "11100000"   = "7"
    | s == "11111110"   = "8"
    | s == "11110110"   = "9"
    | otherwise         = "#"

-- this part is under construction
formFullNum :: [String] -> String
formFullNum = foldl (\x acc -> binaryToDigit x ++ acc) []
--formFullNum ss = foldr (\x acc -> binaryToDigit x ++ acc) []
