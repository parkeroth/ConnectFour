module Main where

import Data.Char
import Board
import AI

data Player = Human
			| CPU
			deriving (Show, Eq)

type PlayerList = [(Char,Player)]

setupState :: GameState
setupState = (newBoard, 'X')

setupPlayers :: IO PlayerList
setupPlayers = do
	putStrLn $ "How many players (1 or 2)?"
	numPlayers <- getChar
	case numPlayers of
		'1' -> return [('X',Human),('O',CPU)]
		'2' -> return [('X',Human),('O',Human)]
		otherwise -> do
				putStrLn ""
				putStrLn "Not a valid number of players!"
				setupPlayers

getUserChoice :: Board -> IO Int
getUserChoice b = do	
				putStrLn "Which column?"
				nextCol <- getChar
				let col = digitToInt nextCol in
					if not (elem nextCol ['1','2','3','4','5','6','7']) then do
						putStrLn ""
					 	putStrLn "Invalid column number!"
					 	getUserChoice b
					 else
					 	if not (validChoice b (col-1)) then do 
					 		putStrLn ""
						 	putStrLn "Column full!"
						 	getUserChoice b
						  else 
					 	return col
					

runGame :: GameState -> PlayerList -> IO Char
runGame (b,t) pList = case lookup t pList of
					Just Human -> do
						showBoard b
						nextCol <- getUserChoice b
						let (b',t') = updateState (b,t) (nextCol - 1) in
							if hasWon (b',t) then
								return t
							 else
								runGame (b',t') pList
					Just CPU -> do
						let (b',t') = updateState (b,t) (nextMove (b,t))  in
							if hasWon (b',t) then do
								showBoard b'
								return t
							 else
								runGame (b',t') pList
					

main = do
	pList <- setupPlayers
	winner <- runGame setupState pList
	putStrLn $ winner : " wins!"
	

