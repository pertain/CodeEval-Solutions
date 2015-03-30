{- fb.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 - It reads in a file where each line contains three (3)
 - space-separated integers. The first value (X) is the
 - first divisor (Fizz), the second value (Y) is the
 - second divisor (Buzz), and the third value is the
 - maximum count (N) (i.e. 1-N).
 -
 - This is the classic FizzBuzz challenge.
 -  >> Begin printing integers from 1 to N
 -  >> If a number is divisible by X, print F (Fizz)
 -  >> If a number is divisible by Y, print B (Buzz)
 -  >> If a number is divisible by X & Y, print FB (FizzBuzz)
 -
 - Example
 -
 -      Input:      2 7 15
 -
 -          X = 2
 -          Y = 7
 -          N = 15
 -
 -      Result:     1 F 3 F 5 F B F 9 11 F 13 FB 15
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ fizzBuzzList linesList

-- convert ByteString to Int (if valid)
bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing -> error "Not an Integer"
    Just (x,_) -> x

-- extract first divisor (X)
getX :: String -> Int
getX = bsToInt . LB.pack . head . words

-- extract second divisor (Y)
getY :: String -> Int
getY = bsToInt . LB.pack . (!! 1) . words

-- extract stopping point (N)
getN :: String -> Int
getN = bsToInt . LB.pack . last . words

-- fixx buzz output for a single value
fizzBuzzValue :: Int -> Int -> Int -> String
fizzBuzzValue n x y
    | mx == 0 && my == 0    = "FB"
    | mx == 0               = "F"
    | my == 0               = "B"
    | otherwise             = show n
    where
        mx = n `mod` x
        my = n `mod` y

-- fizz buzz output for entire line
fizzBuzzLine :: String -> String
fizzBuzzLine ln = unwords $ map (\val -> fizzBuzzValue val x y) [1..n]
    where
        x = getX ln
        y = getY ln
        n = getN ln

-- fizz buzz output for list of lines (entire file)
fizzBuzzList :: [String] -> [String]
fizzBuzzList = map fizzBuzzLine
