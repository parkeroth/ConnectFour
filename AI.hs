module AI where

import System.Random
import Data.List
import Chains
import Position
import Board
	
type Score = Int

--------------------------------------------------------------------------------

nextMove :: BoardState -> Pos
nextMove (b,t) = let col = nextColumn (b,t)
                     row = nextCell b col
                     in (row,col)

nextColumn :: BoardState -> Column
nextColumn (b,t) = let scores = [colScore (b,t) ((nextCell b x),x) | x <- [0..6]]
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

colScore :: BoardState -> Pos -> Int
colScore (b,t) pos = let stop = [2,20,100]
                         make = [1,10,200] in
        sum[(make !! x) * (length (checkFor b pos (Token t) (x+1))) | x <- [0..2]] +
        sum[(stop !! x) * (length (checkFor b pos (Token (swap t)) (x+1))) | x <- [0..2]] +
        sum[1 * (length (checkFor b pos Empty x)) | x <- [0..6]]


--------------------------------------------------------------------------------
-- Util Functions
--------------------------------------------------------------------------------

-- Removes an element a from a list of a
remove :: [a] -> Int -> [a]
remove xs n = let (a,b) = splitAt n xs
				in a ++ (tail b)
