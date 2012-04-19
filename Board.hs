module Board where

import Data.List
import Position

type Board = [[XO]]			
type Chain = (Cell,Int) -- (Type of cell, length of chain)
type Column = Int
type BoardState = (Board,XO)

data XO = X | O
        deriving (Eq,Ord,Show)
        
data Turn	= ToPlay XO
			| HasWon XO
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

--------------------------------------------------------------------------------
-- Construct Chains of Tokens
--------------------------------------------------------------------------------

-- Get a list of all chains at a given position (All 8 directions)
getChainList :: BoardState -> Pos -> [Chain]
getChainList (b,t) pos = let curCell = getCell b pos in
		 [startChain (b,t) (pos,dir) | dir <- [0..7]] 

-- Initiate a chain in a given direction
startChain :: BoardState -> Vector -> Chain
startChain (b,t) (pos,dir) = let firstPos = look (pos,dir)
                                 firstCell = getCell b firstPos
					in case firstCell of
						Invalid -> (Invalid,0)
						Empty 	-> case canPick b firstPos of
										True -> followChain (b,t) (firstPos,dir) firstCell 1
										False -> followChain (b,t) (firstPos,dir) firstCell 0
						Token n -> followChain (b,t) (firstPos,dir) firstCell 1

-- Continue a chain as long as the same token is present				
followChain :: BoardState -> Vector -> Cell -> Int -> Chain
followChain (b,t) (pos,dir) c l = let nextPos = look (pos,dir)
                                      nextCell = getCell b nextPos
							 in case c of
								  Empty -> if canPick b nextPos && nextCell == c then
												followChain (b,t) (nextPos,dir) c (l+1)
											else (c,l)
								  otherwise -> if nextCell == c then
													followChain (b,t) (nextPos,dir) c (l+1)
												else (c,l)

-- Combine any opposite chains into the sum of thier parts
compressChains :: [Chain] -> Cell -> [Chain]
compressChains [] _ = []
compressChains xs c = let (y@(cy,ly):ys) = take (div (length xs) 2) xs 
                          (z@(cz,lz):zs) = drop (div (length xs) 2) xs in
							if c == cy && cy == cz then
								[(c,ly+lz)] ++ (compressChains (ys ++ zs) c)
							 else
								[y] ++ [z] ++ (compressChains (ys ++ zs) c)

--------------------------------------------------------------------------------
-- Check board for a win
--------------------------------------------------------------------------------

-- Determine if the player has successfully created a chain 4 long
-- TODO Return a start and stop pos of winning chain
hasWon :: BoardState -> Maybe Int
hasWon bs = let maxes = [longestChain bs col | col <- [0..6]]
				 in if maximum maxes > 3 then
					  	 elemIndex (maximum maxes) maxes
					 else
						 Nothing

-- Find the longest chain adjacent to a cell						  
longestChain :: BoardState -> Column -> Int
longestChain (b,t) col = let row = topFilled b col
                             chainList = getChainList (b,t) (row,col)
                             centerCell = getCell b (row,col)
                             cleanList = filter (isFilled t) (compressChains chainList centerCell)
                             in	case centerCell of
                             	 Token a -> if a == t then 
                             	 				maxChain (map (\ (c,l) -> (c,l+1)) cleanList) 0
                             	 			 else
                             	 			 	maxChain cleanList 0
                             	 otherwise  -> maxChain cleanList 0

-- Find the maximum of the chains in a given list
maxChain :: [Chain] -> Int -> Int
maxChain [] max 		= max
maxChain ((x,l):xs) max | l > max = maxChain xs l
						| l <= max= maxChain xs max
						
-- Determine if a chain represents a chain of filled spots
isFilled :: XO -> Chain -> Bool
isFilled t chain 	= case chain of 
						((Token i),_) -> (i == t)
						otherwise -> False

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
testBoard1 = [	[],[X,O,O],[O,X,X,O],[O,X,X,X,O,X],
				[O,O],[X,X,O,X],[]]
				
testBoard2 :: Board							
testBoard2 = [	[X],[X,O],[X,O],[O],
				[O],[X],[]]


