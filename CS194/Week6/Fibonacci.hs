--import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 

fibs1 :: [Integer]
fibs1 = map fib [0..]

addTwo :: [Integer] -> [Integer]
addTwo [] 		= []
addTwo [x] 		= [x]
addTwo (x:y:xs) = (x+y):x:y:xs

fibs2 :: [Integer]
fibs2 = foldr _ [] [0..]