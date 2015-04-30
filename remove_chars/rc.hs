{- rc.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 - It reads in a file where each line contains two
 - comma-separated fields. The first is a string, and
 - the second is three characters. The program is to
 - remove all instances of the characters (Field 2)
 - from the string (Field 1), and print the resulting
 - string.
 -
 - Example:
 -
 -      Input:  how are you, abc
 -
 -                  Field 1:    "how are you"
 -                  Field 2:    "abc"
 -
 -      Output: how re you
 -
 - ======================================================
 -
 - TO DO:
 -  ~ refactor rmCharsList so splitOn is only called once
 -
 - ======================================================
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    infile  <- getArgs
    file    <- readFile $ head infile
    let lineslist = lines file
    mapM_ putStrLn $ rmCharsList lineslist

-- remove selected characters from the string
rmChars :: String -> String -> String
rmChars = filter . flip notElem

-- call rmChars on each line in list (input file)
rmCharsList :: [String] -> [String]
rmCharsList = map scrubList
    where
        scrubList ln = rmChars (last $ splitOn ", " ln) (head $ splitOn ", " ln)
