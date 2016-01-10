powersOfTen :: Integer -> [Integer]
powersOfTen n = [10^x | x <- [0..10], n `div` 10^x /= 0]
--The problem with this method is that I don't how to evaluate up
--to the number of powers of 10 that I need for an arbitrary number.
--Some sort of lazy evaluation would be nice.

--Is this method thinking too "imperatively".
--Are there default call values in Haskell?
maxPowerOfTen :: Integer -> Integer -> Integer
maxPowerOfTen n i
	| n `div` 10^i == 0 = 10^(i-1)
	| otherwise 		= maxPowerOfTen n (i+1)
	
maxPowerOfTenExp :: Integer -> Integer -> Integer
maxPowerOfTenExp n i
	| n `div` 10^i == 0 = (i-1)
	| otherwise 		= maxPowerOfTenExp n (i+1)
	
maxPowerOfTenDefault :: Integer -> Integer
maxPowerOfTenDefault n = maxPowerOfTen n 0

maxPowerOfTenExpDefault :: Integer -> Integer
maxPowerOfTenExpDefault n = maxPowerOfTenExp n 0

getCurrentDigit :: Integer -> Integer
getCurrentDigit n = n `div` (maxPowerOfTenDefault n)

removeDigit :: Integer -> Integer
removeDigit n = n - (getCurrentDigit n) * (maxPowerOfTenDefault n)

powerOfTenRemoveDiff :: Integer -> Integer
powerOfTenRemoveDiff n = (maxPowerOfTenExpDefault n) - (maxPowerOfTenExpDefault (removeDigit n))
	
--In this particular application this is okay...but it removes 0 digits
--An ifelse could be added that checks to see if the subtraction removes
--two powers of 10's and then we know that we dropped a zero digit and can 
--add it
toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0 = []
--	| n > 0  = getCurrentDigit : toDigits (removeDigit n)
	| n > 0  = if (powerOfTenRemoveDiff n) > 1
				then getCurrentDigit n : replicate (fromInteger ((powerOfTenRemoveDiff n) - 1)) 0 ++ toDigits (removeDigit n)
				else getCurrentDigit n : toDigits (removeDigit n)
				

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev xs 
	| (length xs) `mod` 2 == 0 = [ x*y | (x, y) <- (zip xs (take (length xs) (cycle [1,2]))) ]
	| otherwise 			   = [ x*y | (x, y) <- (zip xs (take (length xs) (cycle [2,1]))) ]	
	
sumDigits :: [Integer] -> Integer
sumDigits [] 		= 0
sumDigits (x:[]) 	= x
sumDigits (x:xs)	= x + (sumDigits xs)

-- digitzeList is removing zeroes?
-- 0's are replaced by empty lists in toDigits
digitizeList :: [Integer] -> [Integer]
digitizeList [] 		= []
digitizeList (x:[])		= toDigits x ++ []
digitizeList (x:xs)		= toDigits x ++ digitizeList xs

--I get different correct numbers than what's given in the examples.
--Not sure what to make of that.
validate :: Integer -> Bool
validate n 
	| sumDigits (digitizeList (doubleEveryOtherRev (toDigits n))) `mod` 10 == 0 = True
	| otherwise															   		= False