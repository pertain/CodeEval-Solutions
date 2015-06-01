{- fs.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com (EASY).
 - 
 - 
 - https://www.codeeval.com/open_challenges/26/
 -}

import System.Environment (getArgs)
import System.IO

fileSizeInBytes :: FilePath -> IO Integer
fileSizeInBytes file = withFile file ReadMode hFileSize

main :: IO ()
main = do
    inFile <- getArgs
    fileSizeInt <- fileSizeInBytes $ head inFile
    print fileSizeInt
