--module Move (startGame ) where


import Graphs
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

{-PlayerPosition
  Represents the position of a player
  Tuple holding the coordinates
-}
type PlayerPosition = (Float,Float)

-- '
-- data GameInfo = Game { size :: Float
--                       , wallLength = windowSize / gridSize :: Float
--                       , wallRadius = wallLength / 2 :: Float
--                       , x0 = (negate xMax) :: Float
--                       , y0 = (windowSize / 2) :: Float
--                       , xMax = (windowSize / 2) ::Float
--                       , yMax = (negate y0) :: Float
--                       } deriving (Show)   


{-GLOBAL VARIABLES-}
windowSize = 1000  
size = 10 :: Float
gridSize = size
initialWorld = drawing  

wallLength = windowSize / gridSize
wallRadius = wallLength / 2
xMax = (windowSize / 2)
x0 = (negate (xMax))
y0 = (windowSize / 2)
yMax = (negate y0)
windowPadding = 50

background = white :: Color


walls = Graphs.iterDFS $ Graphs.createCells size


{-startGame gameInfo -}
startGame size = undefined


-- till move func
--moveDist = windowSize / gridSize
--startPosFix = moveDist/2




{-createWalls walls
 Creates representation of a walls in a maze
-}
createWalls :: Graphs.Maze -> [Picture]
createWalls [] = []
createWalls (x:xs) =
    singleWall x : createWalls xs
  where
    {-singleWall wallTuple
      Creates representation of a single wall

        RETURNS: Picture showing the wall between cells in 'wallTuple'
        
    -}
    singleWall :: Graphs.Wall -> Picture
    singleWall ((c1_x, c1_y), (c2_x, c2_y)) =
        Line [(l1_x, l1_y), (l2_x, l2_y)]
        where
            x_mid = x0 + wallLength * max c1_x c2_x
            y_mid = y0 - wallLength * max c1_y c2_y
            (l1_x, l2_x, l1_y, l2_y) =
                if (c1_x == c2_x) then
                    (x_mid, x_mid + wallLength, y_mid, y_mid)
                else
                    (x_mid, x_mid, y_mid, y_mid - wallLength)

{-
converts maze into Picture
-}
drawing :: Picture
drawing = pictures (createWalls (walls :: [(Graphs.Cell,Graphs.Cell)]))

--mål shape om en sådan ska användas.
goal :: Picture
goal = (color green (circleSolid x))
  where x = windowSize / (3*gridSize)

{-
Picture with player shape as a solid circle
-}
player:: Picture
player = (color red (circleSolid x))
  where x = windowSize / (2*gridSize)

{-movePlayer dx dy
  Moves the player icon 
-}
movePlayer ::Float -> Float -> Picture
movePlayer x y = Pictures [translate (correctionX x) (correctionY y) (player),initialWorld, goal]
  where
    {-correctioX and correctionY moves the playerdot so it is centerd in the cell.-}
    correctionX x = x0+x*wallLength + wallRadius
    correctionY y = y0-y*wallLength - wallRadius



{-drawPlayfield
  Refreshes a maze with player on a new (or the same) position
    RETURNS:
-}
drawPlayfield :: PlayerPosition -> Picture
drawPlayfield (x,y) = translate 0 0 $ movePlayer x y



{-inputHandler keyStroke world
  
  RETURNS: Updated playfield if keyStroke moves player not through a wall.
  
-}
inputHandler :: Event -> PlayerPosition -> PlayerPosition
inputHandler (EventKey (SpecialKey key) Down _ _ ) (x,y) = case validMove (x,y) direction of 
                                                                                      True -> direction
                                                                                      _    -> (x,y)
  where direction = case key of 
                              KeyUp -> (x, y - 1)
                              KeyDown -> (x, y + 1)
                              KeyRight -> (x + 1, y)
                              KeyLeft -> (x - 1, y)
inputHandler _ (x,y) = (x,y)


{-validMove cell1 cell2
  
    RETURNS: True if there is no wall between cell1 and cell2
    
-}
validMove :: Graphs.Cell -> Graphs.Cell -> Bool
validMove a b@(x,y) 
  | x <  0 = False
  | x >= gridSize = False 
  | y < 0 = False 
  | y >= gridSize = False
  | otherwise = not $ (a,b) `elem` walls || (b,a) `elem` walls 
  

{-windowDisplay
  contains info for windowDisplay in play
-}
windowDisplay :: Display
windowDisplay = InWindow "A Mazing Game" (round windowSize + windowPadding, round windowSize + windowPadding) (100, 500) --

{-
set color of background
-}

{-updateFunc
  No  knows
-}
updateFunc :: Float -> PlayerPosition -> PlayerPosition
updateFunc _ (x, y) = (x, y)

main :: IO ()
main = play
  windowDisplay -- size of window
  background --color
  30 --fps
  (0,0) --Initial World
  drawPlayfield
  inputHandler
  updateFunc