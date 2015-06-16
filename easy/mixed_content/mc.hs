import Data.Char (isNumber)
import Data.List.Split (splitOn)

splitLn :: String -> [String]
splitLn = splitOn ","

--organizeNums :: [String] -> String
organizeNums = foldr (\x acc -> if isNumber (head x) then x : acc else acc) []

--organizeStrings :: [String] -> String
organizeStrings = foldr (\x acc -> if isNumber (head x) then acc else x : acc) []

--organizeLine :: String -> String
organizeLine s = organizeStrings x ++ organizeNums x
    where
        x = splitOn "," s
