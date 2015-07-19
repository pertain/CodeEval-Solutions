{- moan.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Multiples of a Number
 - https://www.codeeval.com/open_challenges/18/
 -}

import System.Environment
import Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ lowestMultAll linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

parseInLine :: String -> [Int]
parseInLine s = map (bsToInt . LB.pack) ss
    where
        ss = splitOn "," s

multsOfN :: [Int] -> [Int]
multsOfN (x:n:_) = takeWhile (\multN -> (multN - n) < x) [n,2*n ..]

lowestMultLine :: String -> Int
lowestMultLine = last . multsOfN . parseInLine

lowestMultAll :: [String] -> [String]
lowestMultAll = map (show . lowestMultLine)
