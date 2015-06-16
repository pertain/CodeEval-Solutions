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


-- this part is under construction
--formFullNum :: [String] -> String
--formFullNum = foldl (\x acc -> binaryToDigit x ++ acc) []
--formFullNum ss = foldr (\x acc -> binaryToDigit x ++ acc) []
--formFullNum = foldr (\x acc -> (binaryToDigit x) ++ acc)


restrictedDigits :: String -> [String]
restrictedDigits [a,b,c,d,e,f,g,h]
    | a == '0'  = ["0","2","3","5","6","7","8","9","0.","2.","3.","5.","6.","7.","8.","9."]
    | b == '0'  = ["0","1","2","3","4","7","8","9","0.","1.","2.","3.","4.","7.","8.","9."] 
    | c == '0'  = ["0","1","3","4","5","6","7","8","9","0.","1.","3.","4.","5.","6.","7.","8.","9."] 
    | d == '0'  = ["0","2","3","5","6","8","9","0.","2.","3.","5.","6.","8.","9."] 
    | e == '0'  = ["0","2","6","8","0.","2.","6.","8."] 
    | f == '0'  = ["0","4","5","6","8","9","0.","4.","5.","6.","8.","9."] 
    | g == '0'  = ["2","3","4","5","6","8","9","2.","3.","4.","5.","6.","8.","9."] 
    | h == '0'  = ["0.","1.","2.","3.","4.","5.","6.","7.","8.","9."] 
    | otherwise = []


willDisplay :: String -> String -> Bool
willDisplay []                                      = False
willDisplay [_]                                     = False
willDisplay [_,_]                                   = False
willDisplay [_,_,_]                                 = False
willDisplay [_,_,_,_]                               = False
willDisplay [_,_,_,_,_]                             = False
willDisplay [_,_,_,_,_,_]                           = False
willDisplay [_,_,_,_,_,_,_]                         = False
willDisplay ['1','1','1','1','1','1',_,'1'] "0."    = True
willDisplay [_,'1','1',_,_,_,_,'1']         "1."    = True
willDisplay ['1','1',_,'1','1',_,'1','1']   "2."    = True
willDisplay ['1','1','1','1',_,_,'1','1']   "3."    = True
willDisplay [_,'1','1',_,_,'1','1','1']     "4."    = True
willDisplay ['1',_,'1','1',_,'1','1','1']   "5."    = True
willDisplay ['1',_,'1','1','1','1','1','1'] "6."    = True
willDisplay ['1','1','1',_,_,_,_,'1']       "7."    = True
willDisplay "11111111"                      "8."    = True
willDisplay ['1','1','1','1',_,'1','1','1'] "9."    = True
willDisplay ['1','1','1','1','1','1',_,_]   "0"     = True
willDisplay [_,'1','1',_,_,_,_,_]           "1"     = True
willDisplay ['1','1',_,'1','1',_,'1',_]     "2"     = True
willDisplay ['1','1','1','1',_,_,'1',_]     "3"     = True
willDisplay [_,'1','1',_,_,'1','1',_]       "4"     = True
willDisplay ['1',_,'1','1',_,'1','1',_]     "5"     = True
willDisplay ['1',_,'1','1','1','1','1',_]   "6"     = True
willDisplay ['1','1','1',_,_,_,_,_]         "7"     = True
willDisplay ['1','1','1','1','1','1','1',_] "8"     = True
willDisplay ['1','1','1','1',_,'1','1',_]   "9"     = True


showDigitValue = map binaryToDigit

