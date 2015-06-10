import qualified Data.List.Split as LS

--revGrps ss = foldr (\x acc -> if valid


validLen ss = length (concat $ LS.splitOn "," ss) `mod` ss == 0


revGrps ss = map reverse (LS.chunksOf 2 $ LS.splitOn "," ss)
