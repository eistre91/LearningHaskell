cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

--list of ints from 1 to n
--remove all
--1 <= i <= j
--i + j + 2ij <= n
--gives odd prime numbers below 2n + 2

findRemovableNumbers :: Integer -> [Integer]
findRemovableNumbers n = map (\(a,b) -> (a + b + 2*a*b)) . filter (\(a,b) -> (a + b + 2*a*b) <= n) . filter (\(a,b) -> a <= b) . cartProd [1..n] $ [1..n]

--filter (\x [a] -> not elem x [a]) [1..n] 
--map (\(a,b) -> (a + b + 2*a*b)) $ findRemovablePairs n

keepComplement :: Integer -> [Integer] -> [Integer]
keepComplement n xs = filter (\x -> not . elem x $ xs) [1..n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) . keepComplement n $ findRemovableNumbers n

--need
--1,1
--1,2 2,2
--1,3 2,3 3,3
--filter ordered pairs
--	\(a,b) -> (a <= b)
--	\n (a,b) -> (a + b + 2*a*b) <= n