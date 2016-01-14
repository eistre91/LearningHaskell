{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

takeOne :: [String] -> String
takeOne = (.) unwords (take 1)

parseMessage :: String -> LogMessage
parseMessage message 
				| code == "I" = LogMessage Info time text
				| code == "W" = LogMessage Warning time text
				| code == "E" = LogMessage (Error priority) time text
				| otherwise	  = Unknown (unwords ws)
	where 
		ws = words message
		code = takeOne ws
		priority = read (takeOne (drop 1 ws)) :: Int
		time = case code of
			"E" -> read (takeOne (drop 2 ws)) :: Int
			_   -> read (takeOne (drop 1 ws)) :: Int
		text = case code of
			"E" -> unwords (drop 3 ws)
			_   -> unwords (drop 2 ws)
		
parse :: String -> [LogMessage]
parse file = parseList (lines file) 

parseList :: [String] -> [LogMessage]
parseList (x:xs) = parseMessage x : parseList xs
parseList []	 = []

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt 							= mt		
insert lm (Leaf) 								= Node Leaf lm Leaf
insert lm@(LogMessage _ timeStamp _) (Node left inner@(LogMessage _ compareTS _) right) 
												| timeStamp < compareTS = Node (insert lm left) inner right
												| otherwise			  	= Node left inner (insert lm right)

lm1 = LogMessage Info 1 "First"
lm2 = LogMessage Info 2 "Second"
lm3 = LogMessage Info 3 "Third"
er1 = LogMessage (Error 51) 4 "Fourth"
er2 = LogMessage (Error 1) 5 "Fifth"
er3 = LogMessage (Error 99) 6 "Sixth"
unk = Unknown "asdf"
blank = Leaf
tree1 = insert lm1 blank
tree2 = insert lm2 tree1
tree3 = insert lm3 tree2
lmList1 = [lm1, lm2, lm3]
lmList2 = [lm2, unk, lm1, lm3]
lmList3 = [lm1, unk, er2, lm3, er1, lm2, er3]

build :: [LogMessage] -> MessageTree
build []		= Leaf
build (x:xs)	= insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder	(Leaf)												= []
inOrder (Node left@(Node _ _ _) inner right@(Node _ _ _)) 	= (inOrder left) ++ [inner] ++ (inOrder right)
inOrder (Node (Leaf) inner right@(Node _ _ _))				= [inner] ++ (inOrder right)
inOrder (Node left@(Node _ _ _) inner (Leaf))				= (inOrder left) ++ [inner]
inOrder (Node (Leaf) inner (Leaf))							= [inner]

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lmList = extractMessages (inOrder (build (throwOut lmList)))

throwOut :: [LogMessage] -> [LogMessage]
throwOut []										= []
throwOut ((keep@(LogMessage (Error i) _ _)):xs) 
												| i >= 50 	= (throwOut xs) ++ [keep]
												| otherwise = throwOut xs
throwOut (x:xs)									= throwOut xs

extractMessages :: [LogMessage] -> [String]
extractMessages [] 						= []
extractMessages ((Unknown m):xs)  		= m : extractMessages xs
extractMessages ((LogMessage _ _ m):xs)	= m : extractMessages xs