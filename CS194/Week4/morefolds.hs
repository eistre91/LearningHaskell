import Data.List

--returns true iff odd number of trues
xor :: [Bool] -> Bool
xor = foldl' (\acc x -> if x == True then (not acc) else acc) (False)

--the below is summed up as:
--acc folded into false does nothing to acc
--(\acc x@(False) -> acc) 
--acc folded into true flips acc
--(\acc x@(True) -> !acc)

--start with acc of false
--false folds into true to give true
	--old number of trues results in true
--true folds into true to give false 
	--even number of trues results in false
--false folds into false to give false
--true folds into false to give true

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

--since we want a list out from foldr, the acc will be an empty list
