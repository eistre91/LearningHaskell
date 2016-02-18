{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char 
 
newtype Score = Score Int
	deriving (Eq, Ord, Show, Num)
	
getScore :: Score -> Int
getScore (Score i) = i
 
instance Monoid Score where
	mempty	= Score 0
	mappend = (+)
	
score :: Char -> Score
score c 
		|toUpper c `elem` worth1	= (Score 1)
		|toUpper c `elem` worth2	= (Score 2)
		|toUpper c `elem` worth3	= (Score 3)
		|toUpper c `elem` worth4	= (Score 4)
		|toUpper c `elem` worth5	= (Score 5)
		|toUpper c `elem` worth8	= (Score 8)
		|toUpper c `elem` worth10	= (Score 10)
		|otherwise					= (Score 0)

scoreString :: String -> Score
scoreString s = sum . map score $ s

worth1 :: [Char]
worth1 = ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U']

worth2 :: [Char]
worth2 = ['D', 'G']

worth3 :: [Char]
worth3 = ['B', 'C', 'M', 'P']

worth4 :: [Char]
worth4 = ['F', 'H', 'V', 'W', 'Y']

worth5 :: [Char]
worth5 = ['K']

worth8 :: [Char]
worth8 = ['J', 'X']

worth10 :: [Char]
worth10 = ['Q', 'Z'] 

--1 points: A E I L N O R S T U
--2 points: D G
--3 points: B C M P
--4 points: F H V W Y
--5 points: K
--8 points: J X
--10 points:Q Z  