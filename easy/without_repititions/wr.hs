{- wr.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Without Repitition
 - https://www.codeeval.com/open_challenges/173/
 -}

import System.Environment (getArgs)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ removeRepsAll linesList

removeRepsLine :: String -> String
removeRepsLine s = reverse rmReps
    where
        revStr  = reverse s
        accBase = [last revStr]
        rmReps  = foldr (\x acc -> if x == head acc then acc else x : acc) accBase revStr

removeRepsAll :: [String] -> [String]
removeRepsAll = map removeRepsLine
