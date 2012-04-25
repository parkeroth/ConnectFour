import Graphics.Blank
import Data.Map (Map)
import qualified Data.Map as Map
import Board
import Chains
import Position
import AI
import Animate

type GameState = (Context,Board,Turn)

--------------------------------------------------------------------------------
-- Alter the game state
--------------------------------------------------------------------------------

updateGameState :: GameState -> Column -> GameState
updateGameState (c,b,turn) col = case turn of
                             HasWon t line -> error "Why would a wookie live on Endor?"
                             ToPlay t -> let b' = updateBoard b t col 
                                             t' = updateTurn b' t in (c,b',t')

-- Update the next turn
updateTurn :: Board -> XO -> Turn
updateTurn b t = case hasWon (b,t) of
                    Nothing -> ToPlay (swap t)
                    Just n -> HasWon t n
                    
hasWon :: BoardState -> Maybe (Pos,Pos)
hasWon (b,t) = let lines = [checkFor b 
                                     ((topFilled b x),x) 
                                     (Token t) 
                                     (getCell b ((topFilled b x),x)) 4 
                           | x <- [0..6]]
                   winners = maximum lines in
                if length(winners) > 0 then
                    Just (head winners)
                 else
                    Nothing
--------------------------------------------------------------------------------

main = blankCanvas 3000 $ \ context -> loop (context,newBoard,ToPlay X)

loop :: GameState -> IO ()
loop gState@(context,board,turn) = do
--        print board
--        print turn
        (width,height,sz) <- send context $ do
            (width,height) <- size
            let sz = min width height
            return (width,height,sz)
                
        send context $ displayBoard board                      
        case turn of
			ToPlay X -> do  
			        event <- send context $ readEvent MouseDown
			        case jsMouse event of
			           Nothing -> loop gState
			           Just pos -> guiColumn pos gState (width,sz)
 			
 			ToPlay O   -> guiCell (nextMove (board,O)) gState
 			HasWon t p -> do  send context $ do
 			                        displayBoard board
 			                        drawWin p
 			                        case t of
 			                            X ->  drawMessage $ "You Won!"
 			                            O ->  drawMessage $ "You Lost!"
 			                       

guiColumn :: (Int,Int) -> GameState -> (Float,Float) -> IO ()
guiColumn (x',y') gs@(context,board,turn) (width, sz) = let x = fromIntegral x'
                                                            y = fromIntegral y' 
                                            in case pointToCol (x,y) board (width,sz) of
						                            Nothing -> loop gs
						                            Just pos -> guiCell pos gs

guiCell :: Pos -> GameState -> IO ()
guiCell pos@(row,col) gs@(context,board,turn) = case getCell board pos of
                                       Empty   -> let newState = updateGameState gs col
                                                    in do sequence_ [ send context cmds
                                                                    | cmds <- animateDrop (board,turn) pos 6
                                                                    ]
                                                          loop newState
                                       Token _ -> loop gs
                                       Invalid -> loop gs

--------------------------------------------------------------------------------
-- Translate click event to a column index
--------------------------------------------------------------------------------

pointToCol :: (Float,Float) -> Board -> (Float,Float) -> Maybe (Int,Int)
pointToCol (x,y) board (width,sz) = do
        x' <- fd ((x - width/2) / sz)
        return (nextCell board x',x')
        
fd x = if r `elem` [0..6] then Just r else Nothing
        where r = round (x / 0.1) + 3       
        
