{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

--data JoinListBasic a = Empty
--						| Single a
--						| Append (JoinListBasic a) (JoinListBasic a)
--						
--jlbToList :: JoinListBasic a -> [a]
--jlbToList Empty			 = []
--jlbToList (Single a)	 = [a]
--jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

import Sized
import Scrabble
import Buffer

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

--listToJl :: [a] -> JoinList m a

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

--creates a new tree
--also has to recompute sizes 
	--use +++ from before?
--given an n, indexJ into n (which is the n+1th element)
--keep that element and everything after

--current implementation loses type information on b
--but that seems to be the nature of the implementation of the Size type
--this doesn't work by the type signature

--size b loses type information too
--so I need to know what's the equivalent of (Size 1) in b
--seems like I need to find the tag of the singles of b and use that
getJoinListAt :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
getJoinListAt _ (Empty) 		= (Empty)
getJoinListAt n l1@(Single m a) 	
				| n < 0 || n >= m 	= (Empty)
				| otherwise			= l1
				where m = getSize . size . tag $ l1
getJoinListAt n l0@(Append m l1 l2) 
				| n < 0 || n >= m 	= (Empty)
				| n < subSize		= getJoinListAt n l1 
				| otherwise			= getJoinListAt (n - subSize) l2
				where 
					subSize = getSize . size . tag $ l1
					m = getSize . size . tag $ l0

--this only computes unit size
tagAt :: (Sized m, Monoid m) => Int -> JoinList m a -> m
tagAt n l1 = tag . getJoinListAt n $ l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l1 = case value of
				Nothing		-> Empty
				Just a		-> (Single (unitSize) a) +++ dropJ (n+1) l1	
			 where 
				unitSize = tagAt 0 l1
				value = indexJ n l1
--basically the reverse of dropJ
--start at keeping n-1 down to 0				
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n l1 = case value of
				Nothing		-> Empty
				Just a		-> takeJ (n-1) l1 +++ (Single (unitSize) a)
			 where 
				unitSize = tagAt 0 l1
				value = indexJ (n-1) l1
--now that I've gone backwards and written getJoinListAt...it makes 
--more sense to use that instead of reconstructing a joinlist object
--where I construct the single

test :: JoinList Size Char
test = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y')
		(Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a')))
		(Single (Size 1) 'h')
		
scoreLine :: String -> JoinList Score String
scoreLine s = (Single (scoreString s) s) 

instance Buffer (JoinList (Score, Size) String) where
  -- | Convert a buffer to a String.
  toString = unwords . jlToList 

  -- | Create a buffer from a String.
  fromString s  = foldl (\acc t -> acc +++ (Single (tag . scoreLine $ t, Size 1) t)) (Empty) (lines s) 

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  line = indexJ 

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine n s l1 = (takeJ (n) l1) +++ (Single (tag . scoreLine $ s, Size 1) s) +++ (dropJ (n+1) l1)

  -- | Compute the number of lines in the buffer.
  numLines = getSize . snd . tag

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value = getScore . fst . tag
  
testBuffer :: JoinList (Score, Size) String
testBuffer = fromString "asdf \n word \n another"