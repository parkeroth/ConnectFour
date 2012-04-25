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
nextColumn (b,t) = let scores = [colScore (b,t) 1 ((nextCell b x),x) | x <- [0..6]]
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

colScore :: BoardState -> Int -> Pos -> Int
colScore (b,t) gen (row,col) =   if gen > 0 then
                                    let b' = updateBoard b t col
                                        scores' = [makeScore b' (swap t) ((nextCell b' x),x) 
                                                  | x <- [0..6]
                                                  ]
                                        m'   = maximum scores'
                                        col' = head (elemIndices m' scores')
                                        in if m' >= 200 && validChoice b' col' then
                                                0
                                            else
                                                calcScore (b,t) (row,col)
                                 else
                                    calcScore (b,t) (row,col)
        
calcScore :: BoardState -> Pos -> Int
calcScore (b,t) pos = (makeScore b t pos) +
                      (stopScore b (swap t) pos) +
                      (emptyScore b pos)
        

makeScore :: Board -> XO -> Pos -> Int
makeScore b t pos = let points = [1,10,200]
                        c = Token t
                        in  sum [(points !! x) * 
                                 (length (checkFor b pos c c (x+2))) 
                                | x <- [0..2]]
                                
stopScore :: Board -> XO -> Pos -> Int
stopScore b t pos = let points = [2,20,100]
                        c = Token t
                        in  sum [(points !! x) * 
                                 (length (checkFor b pos c c (x+2))) 
                                | x <- [0..2]]
                                
emptyScore :: Board -> Pos -> Int
emptyScore b pos = let point = 1 
                       in sum [point * (length (checkFor b pos Empty Empty x)) 
                              | x <- [0..6]]
                        


--------------------------------------------------------------------------------
-- Util Functions
--------------------------------------------------------------------------------

-- Removes an element a from a list of a
remove :: [Int] -> Int -> [Int]
remove (x:xs) 0 = 0:xs
remove (x:xs) n = x:(remove xs (n-1))
