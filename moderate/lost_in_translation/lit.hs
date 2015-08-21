{- lit.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (MODERATE) Lost in Translation
 - https://www.codeeval.com/open_challenges/121/
 -}

import System.Environment (getArgs)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ decipherAll linesList

decipherChar :: Char -> Char
decipherChar orig
    | orig == 'y'   = 'a'
    | orig == 'n'   = 'b'
    | orig == 'f'   = 'c'
    | orig == 'i'   = 'd'
    | orig == 'c'   = 'e'
    | orig == 'w'   = 'f'
    | orig == 'l'   = 'g'
    | orig == 'b'   = 'h'
    | orig == 'k'   = 'i'
    | orig == 'u'   = 'j'
    | orig == 'o'   = 'k'
    | orig == 'm'   = 'l'
    | orig == 'x'   = 'm'
    | orig == 's'   = 'n'
    | orig == 'e'   = 'o'
    | orig == 'v'   = 'p'
    | orig == 'z'   = 'q'
    | orig == 'p'   = 'r'
    | orig == 'd'   = 's'
    | orig == 'r'   = 't'
    | orig == 'j'   = 'u'
    | orig == 'g'   = 'v'
    | orig == 't'   = 'w'
    | orig == 'h'   = 'x'
    | orig == 'a'   = 'y'
    | orig == 'q'   = 'z'
    | orig == ' '   = ' '

decipherString :: String -> String
decipherString = map decipherChar

decipherAll :: [String] -> [String]
decipherAll = map decipherString
