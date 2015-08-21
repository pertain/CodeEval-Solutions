import Data.List (dropWhile)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, ord, chr)

--gronsfeldKey :: String
--gronsfeldKey =  " !\"#$%&'()*+,-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

revGronsfeldKey :: String
revGronsfeldKey =   "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLK\
                    \JIHGFEDCBA@?>=<:9876543210/.-,+*)('&%$#\"! "

--cipher :: String -> [Int]
--cipher s = map digitToInt si
    --where
        --splitLine   = splitOn ";" s
        --si          = head splitLine


--encipheredMsg :: String -> [String]
--encipheredMsg = tail . splitOn ";"


--charShiftPairs :: String -> [Int]
charShiftPairs :: String -> [(Char,Int)]
charShiftPairs s = zip encipheredMsg fullCipher
    where
        splitLn         = splitOn ";" s
        encipheredMsg   = last splitLn
        shortCipher     = map digitToInt (head splitLn)
        n               = length encipheredMsg
        repeatingCipher = concat $ repeat shortCipher
        fullCipher      = take n repeatingCipher

decipherChar :: Char -> Int -> Char
decipherChar ch n = head $ drop n shiftRange
    where
        shiftRange  = dropWhile (ch<) revGronsfeldKey

decipherString :: String -> String
decipherString s = map (uncurry decipherChar) pairs
    where
        pairs   = charShiftPairs s
