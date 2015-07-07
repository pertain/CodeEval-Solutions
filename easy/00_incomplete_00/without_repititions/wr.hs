{- wr.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY)
 - https://www.codeeval.com/open_challenges/173/
 -
 - =============================================
 - THIS IS UNSUBMITTED BECAUSE IT IS NOT CORRECT
 -  
 -  ~ it removes all multiples within a word
 -      when it should only remove contiguous
 -      multiples within a word.
 - =============================================
 -}

import System.Environment (getArgs)
import qualified Data.Set as DS

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ nubAll linesList

nub' :: (Ord a) => [a] -> [a]
nub' = go DS.empty
    where
        go _ [] = []
        go s (x:xs)
            | DS.member x s   = go s xs
            | otherwise         = x : go (DS.insert x s) xs

nubLine :: [String] -> String
nubLine = unwords . map nub'

nubAll :: [String] -> [String]
nubAll = map (nubLine . words)
