import Data.List.Split (splitPlaces)

splitPrefixSuffix :: String -> [String]
splitPrefixSuffix s = splitPlaces [1,length s - 2,1] s
