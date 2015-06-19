{- fs.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Fibonacci Series
 - https://www.codeeval.com/open_challenges/22/
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ allFibs linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

fibList :: [Int]
fibList = 0:1:zipWith (+) fibList (tail fibList)

fibN :: Int -> Int
fibN n = last (take (n+1) fibList)

allFibs :: [String] -> [String]
allFibs = map (show . fibN . bsToInt . LB.pack)
