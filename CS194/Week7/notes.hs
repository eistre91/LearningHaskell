{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty     = 0
treeSum (Node l x r)  = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

--note the common patterns in the above
--abstract out with treefold

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

--rewrite as 
treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

--another example

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

numLiterals :: ExprT -> Int
numLiterals = exprTFold (const 1) (+) (+)

--The fold for a datatype will take one higher order argument
--for each of T's costructors
--which encodes how to turn the value stored by that constructor
--into a value of the result type

--Another type class: Monoids
--class Monoid m where
--    mempty  :: m
--    mappend :: m -> m -> m
--
--    mconcat :: [m] -> m
--    mconcat = foldr mappend mempty
--
--(<>) :: Monoid m => m -> m -> m
--(<>) = mappend
--lists
--numbers under addition
--numbers under multiplication

newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

--note the method of defining unpacking functions
getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)
  
newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)
  
lst :: [Integer]
lst = [1,5,8,23,423,99]

prod :: Integer
prod = getProduct . mconcat . map Product $ lst

--challenge, instance of Monoid for Bool

newtype Any = Any Bool
	deriving(Eq, Show)

instance Monoid (Any) where
	mempty = Any False
	mappend (Any x) (Any y) = Any (x || y)
	
getAny :: Any -> Bool
getAny (Any a) = a

newtype All = All { getAll :: Bool }
	deriving(Eq, Show)

instance Monoid (All) where
	mempty = All True
	mappend (All x) (All y) = All (x && y)

--challenge, make function types an instance of Monoid
	--mempty is the identity
	--mappend is composition
	--mconcat is the full composition
	
