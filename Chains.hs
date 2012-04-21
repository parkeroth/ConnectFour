module Chains where

import Position
import Board

type Chain = (Cell,Int)

--------------------------------------------------------------------------------
-- Construct Chains of Tokens
--------------------------------------------------------------------------------

-- Get a list of all chains at a given position (All 8 directions)
getChainList :: Board -> Pos -> [Chain]
getChainList b pos = let curCell = getCell b pos in
		 [startChain b (pos,dir) | dir <- [0..7]] 

-- Initiate a chain in a given direction
startChain :: Board -> Vector -> Chain
startChain b (pos,dir) = let firstPos = look (pos,dir)
                             firstCell = getCell b firstPos
					in case firstCell of
						Invalid -> (Invalid,0)
						Empty 	-> case canPick b firstPos of
										True -> followChain b (firstPos,dir) firstCell 1
										False -> followChain b (firstPos,dir) firstCell 0
						Token n -> followChain b (firstPos,dir) firstCell 1

-- Continue a chain as long as the same token is present				
followChain :: Board -> Vector -> Cell -> Int -> Chain
followChain b (pos,dir) c l = let nextPos = look (pos,dir)
                                  nextCell = getCell b nextPos
							 in case c of
								  Empty -> if canPick b nextPos && nextCell == c then
												followChain b (nextPos,dir) c (l+1)
											else (c,l)
								  otherwise -> if nextCell == c then
													followChain b (nextPos,dir) c (l+1)
												else (c,l)

--------------------------------------------------------------------------------
-- Chain Search
--------------------------------------------------------------------------------

-- List to search -> size looking for -> possition looking at -> type looking for								
checkFor :: Board -> Pos -> Cell -> Int -> [(Pos,Pos)]
checkFor b pos c l = let    chains = getChainList b pos
                            lengths = map (checkDir c) chains
                            centerCell = getCell b pos in
                          if centerCell == c then      
                              withCenter lengths pos l
                           else
                              withOutCenter lengths pos l
                            
withOutCenter :: [Int] -> Pos -> Int -> [(Pos,Pos)]
withOutCenter [] _ _       = []
withOutCenter (x:xs) pos l = case x == l of
                                True -> let dir = 8 - (length (x:xs)) 
                                            nextPos = look (pos,dir) in
                                        [(nextPos,
                                         (multiLook (nextPos,dir) (l-1))
                                        )] ++ withOutCenter xs pos l
                                False -> withOutCenter xs pos l
                           
withCenter :: [Int] -> Pos -> Int -> [(Pos,Pos)]
withCenter [] _ _   = []
withCenter ls pos n = let l = length ls
                          xs = take (div l 2) ls
                          ys = drop (div l 2) ls
                          x = head xs
                          y = head ys
                          len = 1 + x + y 
                          dirX = 4 - (length xs)
                          dirY = 8 - (length ys)
                          nextX = look (pos,dirX)
                          nextY = look (pos,dirY) in
                       if (len == n) then
                            [(multiLook (pos,dirX) x,
                              multiLook (pos,dirY) y)] ++
                              withCenter ((tail xs) ++ (tail ys)) pos n
                        else
                            getCoord x (n-1) (pos,dirX) ++
                            getCoord y (n-1) (pos,dirY) ++
                            withCenter ((tail xs) ++ (tail ys)) pos n

getCoord :: Int -> Int -> Vector -> [(Pos,Pos)]
getCoord x l (pos,dir) = if x==l then
                              [(pos, multiLook (pos,dir) l)]
                          else 
                              []
                                
checkDir :: Cell -> Chain -> Int
checkDir c (cell,l) = if c == cell then l
                       else 0
