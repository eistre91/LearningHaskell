--import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 

fibs1 :: [Integer]
fibs1 = map fib [0..]

addTwo :: [Integer] -> [Integer]
addTwo t = let 
			rt = reverse t
		   in
			case rt of
				[]			-> []
				[x]			-> [x]
				(x:y:xs)	-> reverse ((x+y):x:y:xs)
				
fibs2 :: [Integer]
fibs2 = foldr (\x acc -> addTwo acc) [0, 1] [0..]

fibs2finite :: Integer -> [Integer]
fibs2finite n = foldr (\x acc -> addTwo acc) [0, 1] [0..n]
