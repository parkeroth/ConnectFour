module Animate where

import Graphics.Blank
import Board
import Position

xColor = "#000000"
oColor = "#ff0000"
boardColor = "#000080"
winColor = "#ffff00"

drawX :: Float -> Canvas ()
drawX radius = do
        beginPath()
        arc(0, 0, radius, 0, 2 * pi, False)
        fillStyle xColor
        fill()
        lineWidth 4
        strokeStyle xColor
        stroke()
        
drawClear :: Float -> Canvas ()
drawClear sz = do
        beginPath()
        fillStyle "#ffffff"
        lineWidth 4
        strokeStyle "#ffffff"
        fillRect(-0.04 * sz, -0.08 * sz, 0.08 * sz, 0.15 * sz)
        stroke()


drawO :: Float -> Canvas ()
drawO radius = do
        beginPath()
        arc(0, 0, radius, 0, 2 * pi, False)
        fillStyle oColor
        fill()
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
        

animateDrop :: (Board,Turn) -> Pos -> Float -> [Canvas ()]
animateDrop (b,t) (row,col) n | n > (fromIntegral row) = case t of
                                ToPlay xo -> do
                                   (width,height,sz) <- getDimensions
                                   save()
                                   translate (width / 2,height / 2)
                                   translate (fromIntegral (col-3) * sz * 0.1,
                                              (n-2) *sz*(-0.1) + 0.05*sz)
                                   drawClear (sz)
                                   translate (0,0.15)
                                   if xo == X then
                                       drawX (sz * 0.025)
                                    else
                                       drawO (sz * 0.025)
                                   restore()
                                   renderLines
--                                   : animateDrop (b,t) (row,col) (1.2*n - 1.22)
                                   : animateDrop (b,t) (row,col) (n - 0.15)
                              | otherwise = []

        
drawWin :: (Pos,Pos) -> Canvas ()
drawWin ((row,col),(row',col')) = do
        (width,height,sz) <- getDimensions
        save()
        translate (width / 2,height / 2)
        beginPath()
        
        moveTo( fromIntegral (col-3)   * (0.1) * sz,
                fromIntegral (row-3)     * (-0.1) * sz - 0.05*sz)
        lineTo( fromIntegral (col'-3)  * (0.1) * sz,
                fromIntegral (row'-3)    * (-0.1) * sz - 0.05*sz)
        
        lineWidth 8
        lineCap "round"
        strokeStyle winColor
        stroke()
        restore()
        
displayBoard :: Board -> Canvas ()
displayBoard board = do
        (width,height) <- size
        clearRect (0,0,width,height)
        beginPath()
        renderLines
        renderTokens board
        
        
renderLines :: Canvas ()
renderLines = do
        (width,height,sz) <- getDimensions
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
        restore()


renderTokens :: Board -> Canvas ()
renderTokens board = do
        (width,height,sz) <- getDimensions
        save()
        translate (width / 2,height / 2)
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
        
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

drawMessage :: String -> Canvas ()
drawMessage str = do
        (width,height,sz) <- getDimensions
        translate (width / 2,height / 2)
        font "20pt Georgia"
        textAlign "center"
        fillText(str, 0, sz*0.4)
        
getDimensions :: Canvas (Float,Float,Float)
getDimensions = do
        (width,height) <- size
        let sz = min width height
        return (width,height,sz)
        
        
