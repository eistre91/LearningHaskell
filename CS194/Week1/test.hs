ex01 = 3 + 2
ex02 = 3 + 3
doubleMe x = x + x
doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
						then x
						else x*2
						
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
evenofxxs = [ [ x | x <- xs, even x ] | xs <- xxs ]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
triangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b] ]
righttriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]
righttriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24 ]