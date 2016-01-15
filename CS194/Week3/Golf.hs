module Golf where

skips :: [a] -> [[a]]
skips list = map (takeNth (zip list [1..])) [1..toInteger (length list)]

takeNth :: [(a, Integer)] -> (Integer -> [a])
takeNth xs n = fst (unzip (filter (\(a, i) -> (i `mod` n == 0)) xs))

testList = [("a",1), ("b",2), ("c", 3), ("d", 4), ("e", 5), ("f", 6)]