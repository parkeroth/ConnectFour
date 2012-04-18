module AI where

import System.Random
import Data.List
import Board
import Position
	
type Score = Int

--------------------------------------------------------------------------------

nextMove :: GameState -> Pos
nextMove (b,t) = let col = nextColumn (b,t)
                     row = nextCell b col
                     in (row,col)

nextColumn :: GameState -> Column
nextColumn (b,t) = let scores = [colScore t (getChainList (b,t) (nextCell b x,x)) | x <- [0..6]]
					 in pickCol b scores

pickCol :: Board -> [Score] -> Column
pickCol b [] = error "Cats Game!"
pickCol b scores = let m  = maximum scores
                       ys = elemIndices m scores
                       n  = head ys
			  	       in if validChoice b n then
			  	       			n
			  	       		else
			  	       			pickCol b (remove scores n)

--------------------------------------------------------------------------------

colScore :: XO -> [Chain] -> Int
colScore t [] = 0
colScore t (x:xs) = chainScore t x + colScore t xs


chainScore :: XO -> Chain -> Int
chainScore t (Invalid,_) 				= 0
chainScore t (Empty,len) 				= 1*len
chainScore t ((Token n),len) 	= let (stop,make) = ([2,20,100],[1,10,200])
										in 	if n == t then
												make !! (len-1)
											 else
												stop !! (len-1)

--------------------------------------------------------------------------------
-- Util Functions
--------------------------------------------------------------------------------

-- Removes an element a from a list of a
remove :: [a] -> Int -> [a]
remove xs n = let (a,b) = splitAt n xs
				in a ++ (tail b)
