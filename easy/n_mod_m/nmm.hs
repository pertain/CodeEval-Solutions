{- nmm.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) N Mod M
 - https://www.codeeval.com/open_challenges/62/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ modAll linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

altMod :: Int -> Int -> Int
altMod n m = n - (m * floor (fromIntegral n / fromIntegral m))

modLine :: String -> Int
modLine s = altMod n m
    where
        inStr   = splitOn "," s
        n       = (bsToInt . LB.pack) (head inStr)
        m       = (bsToInt . LB.pack) (last inStr)

modAll :: [String] -> [String]
modAll = map (show . modLine)
