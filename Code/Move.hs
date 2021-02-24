import Graphs

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import GameInfo

{-
----TODO------
kanter runt spelplan
hål i vägen där "målet är"
ny 
påbörja rapporten
ge upp
-}

{-PlayerPosition
  Represents the position of a player
  Tuple holding the coordinates
-}
type PlayerPosition = (Float,Float)

-- data GameInfo = Game { size :: Float
--                       , wallLength = windowSize / gridSize :: Float
--                       , wallRadius = wallLength / 2 :: Float
--                       , x0 = (negate xMax) :: Float
--                       , y0 = (windowSize / 2) :: Float
--                       , xMax = (windowSize / 2) ::Float
--                       , yMax = (negate y0) :: Float
--                       } deriving (Show)   

{-GLOBAL "VARIABLES"-}
windowSize = GameInfo.window :: Float
size = GameInfo.getSize :: Float
gridSize = size :: Float
initialWorld = Pictures [drawing]  
wallLength = windowSize / gridSize :: Float
wallRadius = wallLength / 2 :: Float
xMax = (windowSize / 2) :: Float
x0 = (negate (xMax)) :: Float
y0 = (windowSize / 2) :: Float
yMax = (negate y0) :: Float
background = red :: Color
walls = Graphs.iterDFS $ Graphs.createCells size -- :: Graphs.Maze

{-
frame makes a frame for maze
-}
frame :: Picture
frame = Line [(x0,yMax),(xMax,yMax),(xMax,y0),(x0,y0),(x0,yMax)]

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
drawing = Pictures (createWalls (walls :: [(Graphs.Cell,Graphs.Cell)]))

--mål shape om en sådan ska användas.
goal :: Picture
goal = (color green (circleSolid x))
  where x = windowSize / (2.5*gridSize)

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
movePlayer x y = Pictures [frame,translate (correctionX x) (correctionY y) (player),initialWorld,translate (correctionX ((size)-1)) (correctionY (size-1)) goal]
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
  
    RETURNS: True if there is no wall between cell1 and cell2 (counting outer edges to be walls).
-}
validMove :: Graphs.Cell -> Graphs.Cell -> Bool
validMove a b@(x,y) = not $ ( x <  0 || x >= gridSize || y < 0 || y >= gridSize || (a,b) `elem` walls || (b,a) `elem` walls )

{-windowDisplay
  contains info for windowDisplay in play
-}
windowDisplay :: Display
windowDisplay = InWindow "A Mazing Game" (((round windowSize) + 200), ((round windowSize) + 200)) (10,10)

{-updateFunc
  returns current playerposition for the next iteration
-}
updateFunc :: Float -> PlayerPosition -> PlayerPosition
updateFunc _ (x, y) = (x,y)
-- updateFunc _ (x, y) 
--   | inGoal (x,y) = Nothing --ingen aning
--   | otherwise = Just (x, y)

inGoal :: PlayerPosition -> PlayerPosition -> Bool
inGoal a b = a == b 

main :: IO ()
main = 
  play
    windowDisplay --windowDisplay -- size of window
    background --color
    30 --fps
    (0,0) --Initial position
    drawPlayfield --A function to convert the world to a picture
    inputHandler --A function to handle individual input events
    updateFunc --Set of functions invoked once per iteration — first argument is the period of time (in seconds) needing to be advanced