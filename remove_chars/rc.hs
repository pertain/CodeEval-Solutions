import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    infile  <- getArgs
    file    <- readFile $ head infile
    let lineslist = lines file
    mapM_ putStrLn $ rmCharsList lineslist

rmChars :: String -> String -> String
rmChars = filter . flip notElem

rmCharsList :: [String] -> [String]
rmCharsList = map scrubList
    where
        scrubList ln = rmChars (last $ splitOn ", " ln) (head $ splitOn ", " ln)
