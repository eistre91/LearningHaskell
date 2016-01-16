module Golf where

import Data.List

skips :: [a] -> [[a]]
skips list = map (takeNth (zip list [1..])) [1..toInteger (length list)]

takeNth :: [(a, Integer)] -> (Integer -> [a])
takeNth xs n = fst (unzip (filter (\(a, i) -> (i `mod` n == 0)) xs))

testList = [("a",1), ("b",2), ("c", 3), ("d", 4), ("e", 5), ("f", 6)]

localMaxima :: [Integer] -> [Integer]
localMaxima []									= []
localMaxima [_]									= []
localMaxima [_, _]								= []
localMaxima (x1:x2:x3:xs) 
						| x2 > x1 && x2 > x3	= x2 : localMaxima (x3:xs)
						| otherwise				= localMaxima (x2:x3:xs)
						
testList2 = [1,2,3,2,1,9,7,4,5,2,1]

--This function isn't perfect since I'm not sure how to fix the spacing of the text output. 
--The size of the = on my screen seem to screw everything up.
--The function works beyond that.
histogram :: [Integer] -> String
histogram list = concat(map (\row -> (concat ((map (\n -> (if n==1 then "*" else " ")) row) ++ ["\n"]))) (generateRows (count list)) ++ ["==========\n0123456789\n"])
										
generateRows :: [Integer] -> [[Integer]] 
generateRows counts 
				| maximum counts == 0	=	[]
				| otherwise				=	atLevel : generateRows (map (\(n, b) -> n - b) (zip counts atLevel))
				where
					atLevel = map (\n -> (n `div` (maximum counts))) counts 
								
count :: [Integer] -> [Integer]
count list = map (\n -> (toInteger (length (filter (\numFromList -> (n == numFromList)) list)))) [0..toInteger 9]

testList3 = [1,2,3,4,5,1,2,3,4,5,1,2,8,9]
--3 1's
--3 2's
