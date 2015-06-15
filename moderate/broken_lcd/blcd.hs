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


{-
-- under construction - need to change the pattern to this style -> ['1','1','0']
willItFit :: String -> String -> Bool
willItFit dig bin
    | bin == (1:1:1:1:1:1:_:1) && dig == "0."   = True
    | bin == (_:1:1:_:_:_:_:1) && dig == "1."   = True
    | bin == (1:1:_:1:1:_:1:1) && dig == "2."   = True
    | bin == (1:1:1:1:_:_:1:1) && dig == "3."   = True
    | bin == (_:1:1:_:_:1:1:1) && dig == "4."   = True
    | bin == (1:_:1:1:_:1:1:1) && dig == "5."   = True
    | bin == (1:_:1:1:1:1:1:1) && dig == "6."   = True
    | bin == (1:1:1:_:_:_:_:1) && dig == "7."   = True
    | bin == (1:1:1:1:1:1:1:1) && dig == "8."   = True
    | bin == (1:1:1:1:_:1:1:1) && dig == "9."   = True
    | bin == (1:1:1:1:1:1:_:_) && dig == "0"    = True
    | bin == (_:1:1:_:_:_:_:_) && dig == "1"    = True
    | bin == (1:1:_:1:1:_:1:_) && dig == "2"    = True
    | bin == (1:1:1:1:_:_:1:_) && dig == "3"    = True
    | bin == (_:1:1:_:_:1:1:_) && dig == "4"    = True
    | bin == (1:_:1:1:_:1:1:_) && dig == "5"    = True
    | bin == (1:_:1:1:1:1:1:_) && dig == "6"    = True
    | bin == (1:1:1:_:_:_:_:_) && dig == "7"    = True
    | bin == (1:1:1:1:1:1:1:_) && dig == "8"    = True
    | bin == (1:1:1:1:_:1:1:_) && dig == "9"    = True
    | otherwise                                 = False
-}

--willItFit :: String -> String -> Bool
willItFit [_,_,_,_,_,_,_,'0']               [_,'.']                 = False
willItFit ['1','1','1','1','1','1',_,'1']   "0."  = True
willItFit [_,'1','1',_,_,_,_,'1']           "1."  = True
willItFit ['1','1',_,'1','1',_,'1','1']     "2."  = True
willItFit ['1','1','1','1',_,_,'1','1']     "3."  = True
willItFit [_,'1','1',_,_,'1','1','1']       "4."  = True
willItFit ['1',_,'1','1',_,'1','1','1']     "5."  = True
willItFit ['1',_,'1','1','1','1','1','1']   "6."  = True
willItFit ['1','1','1',_,_,_,_,'1']         "7."  = True
willItFit ['1','1','1','1','1','1','1','1'] "8."  = True
willItFit ['1','1','1','1',_,'1','1','1']   "9."  = True

willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True
willItFit ['1','1','1','1','1','1','1','1'] "0."  = True


-- this part is under construction
formFullNum :: [String] -> String
--formFullNum = foldl (\x acc -> binaryToDigit x ++ acc) []
--formFullNum ss = foldr (\x acc -> binaryToDigit x ++ acc) []
formFullNum = foldr (\x acc -> (binaryToDigit x) ++ acc) []


showDigitValue = map binaryToDigit


digitToBinary :: String -> String
digitToBinary s
    | s == "0."  = "11111101"
    | s == "1."  = "01100001"
    | s == "2."  = "11011011"
    | s == "3."  = "11110011"
    | s == "4."  = "01100111"
    | s == "5."  = "10110111"
    | s == "6."  = "10111111"
    | s == "7."  = "11100001"
    | s == "8."  = "11111111"
    | s == "9."  = "11110111"
    | s == "0"   = "11111100"
    | s == "1"   = "01100000"
    | s == "2"   = "11011010"
    | s == "3"   = "11110010"
    | s == "4"   = "01100110"
    | s == "5"   = "10110110"
    | s == "6"   = "10111110"
    | s == "7"   = "11100000"
    | s == "8"   = "11111110"
    | s == "9"   = "11110110"
    | otherwise     = "#"
