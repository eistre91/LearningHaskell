class Listable a where
  toList :: a -> [Integer]
  
instance Listable Integer where
  toList x = [x]
  
--instance Num a => Listable a where
--  toList x = [x]
  
instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y