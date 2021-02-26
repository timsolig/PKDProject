
import Move
import Graphs
import Render
import Data.Time
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

--import Graphics.Gloss
--import Graphics.Gloss.Interface.Pure.Game


{-window
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
window :: Display
window = InWindow "A Mazing Game" (round windowSize + 200, round windowSize + 200) (10, 10)

{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
background :: Color
background = white

{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
fps :: Int
fps = 30

{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
initialState :: Picture -> Picture -> GameState
initialState p1 p2 = Game {
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
        startTime    = getTime playerLevel,
        timeNow      = getTime playerLevel
    }

main :: IO ()
main = do
    p1 <- loadBMP "./Icons/1.bmp"
    p2 <- loadBMP "./Icons/2.bmp"
    play window background fps (initialState p1 p2) render handleKeys (const id)