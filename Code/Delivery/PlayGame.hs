import Move

import Render

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{-func 
  func descirption
    PRE:
    RETURNS:
    EXAMPLES:
-}
initialState :: Picture -> Picture -> GameState
initialState p1 p2= Game {
        startMenu    = True,
        goalMenu     = False,
        gridSize     = undefined,
        mazePicture  = undefined,
        walls        = undefined,
        playerCoords = undefined,
        playerLevel  = undefined,
        goalCoords   = undefined,
        steps        = undefined,
        testImageP   = p1,
        testImageG   = p2,
        seconds      = undefined
    }

main :: IO ()
main = do
    --Loads pictures
    p1 <- loadBMP "2.bmp"
    p2 <- loadBMP "2.bmp"
    play window background fps (initialState p1 p2) render handleKeys counter