{- ad.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Age Distribution
 - https://www.codeeval.com/open_challenges/152/
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ allAgeGroups linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

ageDist :: Int -> String
ageDist i
    | i > 100 || i < 0  = "This program is for humans"
    | i > 65            = "The Golden Years"
    | i > 22            = "Working for the man"
    | i > 18            = "College"
    | i > 14            = "High school"
    | i > 11            = "Middle school"
    | i > 4             = "Elementary school"
    | i > 2             = "Preschool Maniac"
    | i >= 0            = "Still in Mama's arms"

allAgeGroups :: [String] -> [String]
allAgeGroups = map (ageDist . bsToInt . LB.pack)
