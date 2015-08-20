import Data.Char (isUpper, toLower, toUpper)

swapCase :: String -> String
swapCase = foldr (\x acc -> if isUpper x then toLower x : acc else toUpper x : acc) []
