import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul i1 i2) = eval i1 * eval i2
eval (Add i1 i2) = eval i1 + eval i2

--how to implement foldable?
--not sure how to do that but that seems like a 
--reasonable implementation here

evalStr :: String -> Maybe Integer
evalStr expr = case parseExp Lit Add Mul expr of
	(Just e)	-> Just (eval e)
	(Nothing)	-> Nothing
	
class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a
	
instance Expr ExprT where 
	lit x 	= Lit x
	add x y = Add x y
	mul x y = Mul x y
	
instance Expr Integer where
	lit x	= x
	add x y = x + y
	mul x y = x * y
	
instance Expr Bool where
	lit x 	= if x <= 0 then False else True
	add x y = x || y
	mul x y = x && y

newtype MinMax	= MinMax Integer deriving (Eq, Show)
newtype Mod7	= Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
	lit x						= MinMax x
	add (MinMax x) (MinMax y)	= MinMax (max x y)
	mul (MinMax x) (MinMax y) 	= MinMax (min x y)
	
instance Expr Mod7 where
	lit x					= Mod7 (x `mod` 7)
	add (Mod7 x) (Mod7 y) 	= Mod7 ((x + y) `mod` 7)
	mul (Mod7 x) (Mod7 y)	= Mod7 ((x * y) `mod` 7)
	
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

