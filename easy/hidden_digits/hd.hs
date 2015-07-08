{- hd.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Hidden Digits
 - https://www.codeeval.com/open_challenges/122/
 -}

import System.Environment (getArgs)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ printAllLines linesList

validChars = "abcdefghij0123456789"

parseNums :: String -> String
parseNums = filter (`elem` validChars)

decipherChar :: Char -> Char
decipherChar c
    | c == 'a' || c == '0'  = '0'
    | c == 'b' || c == '1'  = '1'
    | c == 'c' || c == '2'  = '2'
    | c == 'd' || c == '3'  = '3'
    | c == 'e' || c == '4'  = '4'
    | c == 'f' || c == '5'  = '5'
    | c == 'g' || c == '6'  = '6'
    | c == 'h' || c == '7'  = '7'
    | c == 'i' || c == '8'  = '8'
    | c == 'j' || c == '9'  = '9'

printLine :: String -> String
printLine s
    | null s    = "NONE"
    | otherwise = foldr (\x acc -> decipherChar x : acc ) [] s

printAllLines :: [String] -> [String]
printAllLines = map (printLine . parseNums)
