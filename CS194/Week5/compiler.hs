{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import StackVM
import Parser

class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a
	
instance Expr Program where
	lit x		= [PushI x]
	add x y		= x ++ y ++ [Add]
	mul x y		= x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

--"3+5" should give back add (lit 3) (lit 5)
	--this part comes from parseExp
--which should give back [PushI 3, PushI5, Add]
	--this comes from getting the result from parseExp
	--and typing the resulting Expr as Program