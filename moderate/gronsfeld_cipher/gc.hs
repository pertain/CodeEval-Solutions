{- gc.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (MODERATE) Gronsfeld Cipher
 - https://www.codeeval.com/open_challenges/181/
 -}

import System.Environment (getArgs)
import Data.List (dropWhile)
import Data.List.Split (splitOn)
import Data.Char (digitToInt, ord, chr)

--gronsfeldKey :: String
--gronsfeldKey =  " !\"#$%&'()*+,-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

revGronsfeldKey :: String
revGronsfeldKey =   "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLK\
                    \JIHGFEDCBA@?>=<:9876543210/.-,+*)('&%$#\"! \
                    \zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLK\
                    \JIHGFEDCBA@?>=<:9876543210/.-,+*)('&%$#\"! "

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ decipherAll linesList

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

decipherAll :: [String] -> [String]
decipherAll = map decipherString
