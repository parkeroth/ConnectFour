module GUI where

import Graphics.Blank
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Board
import Position
import AI

type Dims = (Float,Float,Float)

main = blankCanvas 3000 $ \ context -> loop context newBoard X

loop :: Context -> Board -> XO -> IO ()
loop context board turn = do
--        print board
--        print turn
        (width,height,sz) <- send context $ do
                (width,height) <- size
                clearRect (0,0,width,height)
                beginPath()

                let sz = min width height
                save()
                translate (width / 2,height / 2)
                sequence_ [ do bigLine (n,-sz * 0.3) (n,sz * 0.3) -- Vertical Lines
                          | n <- [ -sz * 0.35, -sz * 0.25, -sz * 0.15, -sz * 0.05,
                          		   sz * 0.05,  sz * 0.15,  sz * 0.25,  sz * 0.35]
                          ]
                          
                sequence_ [ do bigLine (-sz * 0.35,n) (sz * 0.35,n) -- Horizontal Lines
                          | n <- [ -sz * 0.3, -sz * 0.2, -sz * 0.1, 0, 
                          			sz * 0.1,  sz * 0.2,  sz * 0.3]
                          ] 


                sequence_ [ do save()
                               translate (fromIntegral (x-3) * sz * 0.1,
                               			  fromIntegral (y-2) *sz*(-0.1) + 0.05*sz)
                               case getCell board (y,x) of
                                  Token X -> drawX (sz * 0.025)
                                  Token O -> drawO (sz * 0.025)
                                  Empty -> return ()
                                  Invalid -> return ()
                               restore()
                          | x <- [0..6]
                          , y <- [0..5]
                          ]
                restore()
                return (width,height,sz)
                              
        case turn of
			X -> do
				event <- send context $ readEvent MouseDown
				case jsMouse event of
				   Nothing -> loop context board turn
				   Just pos -> guiColumn pos context board turn (width,height,sz)
 			
 			O -> guiCell (nextMove (board,turn)) context board turn
 			

guiColumn :: (Int,Int) -> Context -> Board -> XO -> Dims -> IO ()
guiColumn (x',y') context board turn dims = let x = fromIntegral x'
                                                y = fromIntegral y' in
                                            case pointToCol (x,y) board dims of
						                        Nothing -> loop context board turn
						                        Just pos -> guiCell pos context board turn

guiCell :: Pos -> Context -> Board -> XO -> IO ()
guiCell (row,col) context board turn = case getCell board (row,col) of
                                        Empty -> let newBoard = updateBoard board turn col 
                                                     nextTurn = swap turn in
                                                 loop context newBoard nextTurn
                                        Token _ -> loop context board turn
                                        Invalid -> loop context board turn

--------------------------------------------------------------------------------
-- Translate click event to a column index
--------------------------------------------------------------------------------

pointToCol :: (Float,Float) -> Board -> Dims -> Maybe (Int,Int)
pointToCol (x,y) board (width,height,sz) = do
        x' <- fd ((x - width/2) / sz)
        return (nextCell board x',x')
        
fd x = if r `elem` [0..6] then Just r else Nothing
        where r = round (x / 0.1) + 3

--------------------------------------------------------------------------------
-- Draw Methods
--------------------------------------------------------------------------------

xColor = "#ff0000"
oColor = "#00a000"
boardColor = "#000080"

drawX :: Float -> Canvas ()
drawX size = do
        strokeStyle xColor
        lineCap "butt"
        beginPath()
        moveTo(-size,-size)
        lineTo(size,size)
        lineWidth 4
        stroke()
        beginPath()
        moveTo(-size,size)
        lineTo(size,-size)
        lineWidth 4
        stroke()

drawO :: Float -> Canvas ()
drawO radius = do
        beginPath()
        arc(0, 0, radius, 0, 2 * pi, False)
        lineWidth 4
        strokeStyle oColor
        stroke()

bigLine :: (Float,Float) -> (Float,Float) -> Canvas ()
bigLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 8
        lineCap "round"
        strokeStyle boardColor
        stroke()

