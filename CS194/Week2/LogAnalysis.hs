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
			_ ->  read (takeOne (drop 1 ws)) :: Int
		text = case code of
			"E" -> unwords (drop 3 ws)
			_ -> unwords (drop 2 ws)
		
parse :: String -> [LogMessage]
parse file = parseList (lines file) 

parseList :: [String] -> [LogMessage]
parseList (x:xs) = parseMessage x : parseList xs
parseList []	 = []