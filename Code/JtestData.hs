--module Move (test) where

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

data GameInfo = GameInfo { size' :: Float
                      , windowSize' :: Float
                      , playerPosition :: (Float,Float)
                      , goalPosition :: (Float,Float)
                      , maze :: Picture
                      , walls :: ----------------------------------------
                      } deriving (Show)   



getSize' :: GameInfo -> Float
getSize' (GameInfo x _ _ _ _) = x

setSize' :: GameInfo -> Float -> GameInfo
setSize' (GameInfo _ y z q r) newSize = GameInfo newSize y z q r

getWindowSize :: GameInfo -> Float
getWindowSize (GameInfo _ x _ _ _) = x

setWindowSize :: GameInfo -> Float -> GameInfo
setWindowSize (GameInfo x _ z q r) newWindowSize = GameInfo x newWindowSize z q r

getPlayerPosition :: GameInfo -> (Float,Float)
getPlayerPosition (GameInfo _ _ x _ _) = x

setPlayerPosition :: GameInfo -> (Float,Float) -> GameInfo
setPlayerPosition (GameInfo x z _ q r) newPlayerPosition = GameInfo x z newPlayerPosition q r


getGoalPosition :: GameInfo -> (Float,Float)
getGoalPosition (GameInfo _ _ _ x _) = x

setGoalPosition :: GameInfo -> (Float,Float) -> GameInfo
setGoalPosition (GameInfo x z q _ r) newGoalPosition = GameInfo x z q newGoalPosition r

getMaze :: GameInfo -> Picture
getMaze (GameInfo _ _ _ _ x) = x

setMaze :: GameInfo -> GameInfo
setMaze (GameInfo x z q r _) = GameInfo x z q r (initialWorld) 

updateMaze :: GameInfo -> GameInfo
updateMaze (GameInfo x z q r _) = GameInfo x z q r (drawing x) 

{-GLOBAL "VARIABLES"-}
windowSize = 1000 :: Float
size = 10:: Float
gridSize = size :: Float

initialWorld = Pictures [drawing 10]  


wallLength = windowSize / gridSize :: Float
wallRadius = wallLength / 2 :: Float
xMax = (windowSize / 2) :: Float
x0 = (negate (xMax)) :: Float
y0 = (windowSize / 2) :: Float
yMax = (negate y0) :: Float
background = blue :: Color

walls x = Graphs.iterDFS $ Graphs.createCells x -- :: Graphs.Maze

{-
frame makes a frame for maze
-}
frame :: Picture
frame = Line [(x0,yMax),(xMax,yMax),(xMax,y0),(x0,y0),(x0,yMax)]

{-createWalls walls
 Creates representation of a walls in a maze
-}
createWalls :: Graphs.Maze -> [Picture]
createWalls  [] = []
createWalls  (x:xs) =
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
drawing :: Float -> Picture
drawing x = Pictures (createWalls (walls x :: [(Graphs.Cell,Graphs.Cell)]))

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
movePlayer ::GameInfo -> Picture
movePlayer gameInfo = 
                    let (x,y) = getPlayerPosition gameInfo
                        maze = getMaze gameInfo
                        size = getSize' gameInfo
                    in
                        Pictures [frame,translate (correctionX x) (correctionY y) (player),maze ,translate (correctionX ((size)-1)) (correctionY (size-1)) goal]
                            where
                              {-correctioX and correctionY moves the playerdot so it is centerd in the cell.-}
                              correctionX x = x0+x*wallLength + wallRadius
                              correctionY y = y0-y*wallLength - wallRadius

{-drawPlayfield
  Refreshes a maze with player on a new (or the same) position
    RETURNS:
-}
drawPlayfield :: GameInfo -> Picture
drawPlayfield gameInfo = {-translate 0 0 $-} movePlayer gameInfo


{-inputHandler keyStroke world
  
  RETURNS: Updated playfield if keyStroke moves player not through a wall.
  
-}
inputHandler :: Event -> GameInfo -> GameInfo
inputHandler (EventKey (SpecialKey key) Down _ _ ) gameInfo = 
                                                            let (x,y) = getPlayerPosition gameInfo
                                                                direction = case key of 
                                                                                    KeyUp -> (x, y - 1)
                                                                                    KeyDown -> (x, y + 1)
                                                                                    KeyRight -> (x + 1, y)
                                                                                    KeyLeft -> (x - 1, y)
                                                            in 
                                                                case validMove gameInfo (x,y) direction of 
                                                                                      True -> setPlayerPosition gameInfo direction 
                                                                                      _    -> gameInfo
                                                                    
inputHandler _ x = x


{-validMove cell1 cell2
  
    RETURNS: True if there is no wall between cell1 and cell2 (counting outer edges to be walls).
    
-}-- (Float,Float)
validMove :: GameInfo -> Graphs.Cell -> Graphs.Cell -> Bool
validMove gameInfo a b@(x,y) = not $ ( x <  0 || x >= gridSize || y < 0 || y >= gridSize || (a,b) `elem` (walls (getSize' gameInfo)) || (b,a) `elem` (walls (getSize' gameInfo)) )


{-windowDisplay
  contains info for windowDisplay in play
-}
windowDisplay :: Display
windowDisplay = InWindow "A Mazing Game" (((round windowSize) + 200), ((round windowSize) + 200)) (10,10)

{-updateFunc
  returns current playerposition for the next iteration
-}

{-
updateFunc :: Float -> PlayerPosition -> PlayerPosition
updateFunc _ (x, y) = (x,y)
-- updateFunc _ (x, y) 
--   | inGoal (x,y) = Nothing --ingen aning
--   | otherwise = Just (x, y)
-}
--Testar att ha ett "mål"
updateFunc :: Float -> GameInfo -> GameInfo
updateFunc _ x 
   | inGoal (getPlayerPosition x) (getGoalPosition x) = updateMaze (setSize' (setPlayerPosition x (0,0)) ((getSize' x)*2))
   | otherwise = x
  --  | om den är framme = skicka till annan funktino som skapar ny gameInfo som skickar till drawPlayerfiled


inGoal :: (Float,Float) -> (Float,Float) -> Bool
inGoal a b = a == b 


main ::IO ()
main =
    let gameInfo = (GameInfo 10 1000 (0,0) (20,20) (drawing 10))
        in
            play
              windowDisplay --windowDisplay -- size of window
              background --color
              10 --fps
              gameInfo --Initial position
              drawPlayfield --A function to convert the world to a picture
              inputHandler --A function to handle individual input events
              updateFunc --Set of functions invoked once per iteration — first argument is the period of time (in seconds) needing to be advanced
      

      
