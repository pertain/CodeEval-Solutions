import Data.Char (toUpper, toLower, isletter)

capsBinary :: String -> String
capsBinary s = take (length s) (cycle "10")

prepareLine :: String -> [(String,String)]
prepareLine s = zip ss bs
    where
        ss = words s
        bs = map capsBinary ss

prepareWords :: [(String,String)] -> [[(Char,Char)]]
prepareWords = map (uncurry zip)
