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

data List t = E | C2 t (List t)
	deriving (Show)

lst1 :: List Int
lst1 = C2 3 (C2 5 (C2 2 E))

data Stream a = C a (Stream a)

streamToList :: Stream a -> [a]
streamToList (C x y) = x:(streamToList y)

instance Show a => Show (Stream a) where
	show x = show . take 20 . streamToList $ x

streamRepeat :: a -> Stream a	
streamRepeat x = C x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (C x y) = C (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = C (seed) (streamFromSeed f (f seed))
	
nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

ruler :: Stream Integer	
ruler = streamMap determinePowerOfTwo nats
--works in theory but it never prints anything because infinite
--not sure how to get partial results put out

determinePowerOfTwo :: Integer -> Integer
determinePowerOfTwo n = if n `mod` 2 == 0 then (1 + determinePowerOfTwo (n `div` 2)) else 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (C first tail1) (C second tail2) = C first (C second (interleaveStreams tail1 tail2))
--can't quite figure out the interleave solution yet

--st1 :: Stream Int
--st1 = C 0 (Stream 0)
--Note this doesn't actually work.
--Stream itself isn't a constructor, but C is.
--Can't type out the construction of a Stream.
