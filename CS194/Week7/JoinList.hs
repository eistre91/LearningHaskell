--data JoinListBasic a = Empty
--						| Single a
--						| Append (JoinListBasic a) (JoinListBasic a)
--						
--jlbToList :: JoinListBasic a -> [a]
--jlbToList Empty			 = []
--jlbToList (Single a)	 = [a]
--jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

import Sized

data JoinList m a = Empty
					| Single m a
					| Append m (JoinList m a) (JoinList m a)
		deriving (Eq, Show)
		
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x	 							= x
(+++) x Empty	 							= x
(+++) l1@(Single m1 _) l2@(Single m2 _)		= Append (m1 `mappend` m2) l1 l2 
(+++) l1@(Single m1 _) l2@(Append m2 _ _)	= Append (m1 `mappend` m2) l1 l2
(+++) l1@(Append m1 _ _) l2@(Single m2 _) 	= Append (m1 `mappend` m2) l1 l2
(+++) l1@(Append m1 _ _) l2@(Append m2 _ _)	= Append (m1 `mappend` m2) l1 l2

--getting in two join lists and I want to just append them
--I want to compute the new annotation then through in l1 l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

--added from document for aid with testing

(!!?) :: [a] -> Int -> Maybe a
[]		!!? _			= Nothing
_		!!? i | i < 0 	= Nothing
(x:xs)  !!? 0			= Just x
(x:xs) 	!!? i			= xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty			 = []
jlToList (Single _ a)	 = [a]
jlToList (Append _ l1 l2)= jlToList l1 ++ jlToList l2

--finish add from document
--should have that (indexJ i jl) == (jlToList jl !!? i)

--find the JoinList element at the specified indexJ
--Nothing if the index is out of bounds
--index in the list it represents
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Empty) 		= Nothing
indexJ n l1@(Single m a) 	
				| n < 0 || n >= m 	= Nothing
				| otherwise			= Just a
				where m = getSize . size . tag $ l1
indexJ n l0@(Append m l1 l2) 
				| n < 0 || n >= m 	= Nothing
				| n < subSize		= indexJ n l1 
				| otherwise			= indexJ (n - subSize) l2
				where 
					subSize = getSize . size . tag $ l1
					m = getSize . size . tag $ l0

test :: JoinList Size Char
test = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y')
		(Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a')))
		(Single (Size 1) 'h')