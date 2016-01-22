data Tree a = 	Leaf
				| Node Integer (Tree a) a (Tree a)
			deriving (Show, Eq)
			

foldTree :: [a] -> Tree a

balancedPlacement :: a -> Tree a -> Tree a
balancedPlacement x Leaf = Node 0 Leaf x Leaf
balancedPlacement x (Node h left@(Node lh _ _ _) value right@(Node rh _ _ _))
			| lh < rh && countChildren	= Node h+1 (balancedPlacement x left) value right
			| otherwise = Node h+1 left value (balancedPlacement x right)
--also have the case x (Node h Leaf value right)
--and the reverse 	 x (Node h left value Leaf)

countNodes :: Tree a -> Integer
countNodes Leaf			= 0
countNodes (Node h left _ right)
		| h == 0	= 1
		| otherwise = 1 + countNodes left + countNodes right
--a generalized fold for trees would make this function nicer

countChildren :: Tree a -> Integer
countChildren xs = countNodes xs - 1 

maxChildren :: Tree a -> Integer
maxChildren Leaf 			= 0
maxChildren (Node h _ _ _)	 
		| h == 0 	= 0 
		| otherwise = sum . takeWhile(<=2^h) . map (2^) $ [1..]

--foldr
--accumulating a tree
--foldr func acc list

--first accumulator is leaf

--need a function that takes a current tree and places in
--new values in a balanced way

--finding balanced placement:
	--take current node, compare both children's height
	--go to the one that is less
	--repeat
	--change height as I pass through nodes
		--this doesn't work universally
		--only increment height if it has more children than its possible max