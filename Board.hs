module Board where

import Data.List
import Position

type Board = [[XO]]			

type Column = Int
type BoardState = (Board,XO)

data XO = X | O
        deriving (Eq,Ord,Show)
        
data Turn	= ToPlay XO
			| HasWon XO (Pos,Pos)
			deriving (Eq,Show)
			
data Cell 	= Token XO
			| Empty
			| Invalid 
			deriving (Eq)
			
instance Show Cell where
	show (Token x) 	= show x
	show (Empty)	= " "
	show (Invalid) 	= "!"
			
swap :: XO -> XO
swap X = O
swap O = X

newBoard :: Board
newBoard = [[] | x <- [1..7]]

-- Update the board with the latest move
updateBoard :: Board -> XO -> Column -> Board
updateBoard b t col = [if i==col then c ++ [t] else c
                      | (c,i) <- zip b [0..]
                      ]
                        

--------------------------------------------------------------------------------
-- Board Queries
--------------------------------------------------------------------------------

nextCell :: Board -> Int -> Int
nextCell b col = length (b !! col)

topFilled :: Board -> Int -> Int
topFilled b col = let l = length (b !! col)
					in case l of
						0 -> 0
						otherwise -> l-1
					

filled :: Board -> Column -> Bool
filled b col = length (b !! col) >= 6

isEmpty :: Board -> Pos -> Bool
isEmpty b (row,col) = length (b !! col) <= row

canPick :: Board -> Pos -> Bool
canPick b (row,col) = if isValid (row,col) then
						length (b !! col) == row
					   else
						False

getCell :: Board -> Pos -> Cell
getCell b (row,col) = case isValid (row,col) of
						False -> Invalid
						True  -> case (isEmpty b (row,col)) of
							True  -> Empty
							False -> Token $ ((b !! col ) !! row)
							
validChoice :: Board -> Column -> Bool
validChoice b col = let row = nextCell b col in
						row < 6
							
--------------------------------------------------------------------------------
-- Test Items
--------------------------------------------------------------------------------							

testBoard1 :: Board							
testBoard1 = [[O,X,O,X,O,O],[O,X,O,X,X,O],[O,X,X],[X,O,X,O,X,O],[X,X,O,O,X,X],[O,X,O,X,O,X],[X,O,X,O,X,O]]
				
testBoard2 :: Board							
testBoard2 = [[X,O,X,O,O,O],[O,X,X,O,X],[O,X,O,X,O,O],[X,X,O,X,X,O],[X,O,X,O],[O,X],[O,X,X,O,X]]


