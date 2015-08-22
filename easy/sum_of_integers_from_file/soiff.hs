{- soiff.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Sum of Integers from File
 - https://www.codeeval.com/open_challenges/24/
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    sumAllLines linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

sumAllLines :: [String] -> IO ()
sumAllLines = print . sum . map (bsToInt . LB.pack)
