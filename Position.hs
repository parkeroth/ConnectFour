module Position where

type Dir = Int
type Pos = (Int, Int) -- (Row number [0..5], Column number [0..6])
type Vector = (Pos,Dir)

look :: Vector -> Pos
look ((row,col),dir) = case dir of
						0 -> (row-1,col-1)
						1 -> (row,	col-1)
						2 -> (row+1,col-1)
						3 -> (row+1,col)
						4 -> (row+1,col+1)
						5 -> (row,	col+1)
						6 -> (row-1,col+1)
						7 -> (row-1,col)

isValid :: Pos -> Bool
isValid pos = checkRow pos && checkColumn pos

checkRow :: Pos -> Bool
checkRow (x,_) = x >= 0 && x < 6

checkColumn :: Pos -> Bool
checkColumn (_,x) = x >= 0 && x < 7
