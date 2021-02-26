module Render where

import Graphs 

import Data.Time
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

{-GameState
  Represents the state of the game at a current "state" :D
-}
data GameState = Game {
        startMenu    :: Bool,
        goalMenu     :: Bool,
        gridSize     :: Float,
        mazePicture  :: Picture,
        walls        :: Graphs.Maze,
        playerCoords :: (Float, Float),
        playerLevel  :: Int,
        goalCoords   :: (Float, Float),
        steps        :: Int,
        testImageP   :: Picture,
        testImageG   :: Picture,
        startTime    :: UTCTime,
        timeNow      :: UTCTime
    }

{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
windowSize, xMax, x0, yMax, y0 :: Float
windowSize = 1000
xMax = windowSize / 2
x0 = negate $ windowSize / 2
y0 = windowSize / 2
yMax = negate $ windowSize / 2


{-func
  Draws the walls in a maze
    PRE: Ingenting va?
    RETURNS:
    EXAMPLES:
-}
drawWall :: Maze -> Float -> [Picture]
drawWall [] _ = []
drawWall (x:xs) gridSize =
    singleWall x gridSize : drawWall xs gridSize
  where
    singleWall :: Wall -> Float -> Picture
    singleWall ((c1_x, c1_y), (c2_x, c2_y)) gridSize =
        Line [(l1_x, l1_y), (l2_x, l2_y)]
        where
            wallLength = windowSize / gridSize
            x_mid = x0 + wallLength * max c1_x c2_x
            y_mid = y0 - wallLength * max c1_y c2_y
            (l1_x, l2_x, l1_y, l2_y) =
                if (c1_x == c2_x) then
                    (x_mid, x_mid + wallLength, y_mid, y_mid)
                else
                    (x_mid, x_mid, y_mid, y_mid - wallLength)

{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
createMaze :: Float -> Maze
createMaze gridSize = foo gridSize


{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
wallPicture :: Maze -> Float -> Picture
wallPicture wall gridSize = Pictures (drawWall wall gridSize)



{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
gridCalculate :: (Float, Float) -> Float -> (Float, Float)
gridCalculate (x, y) gs = 
    let wallLength = windowSize / gs
    in (x0 + (x + 0.5) * wallLength, y0 - (y + 0.5) * wallLength)


{-outerEdge n
  Creates pictur ef the outer edge of the maze
    PRE: n >= 0
    RETURNS:

-}
outerEdge :: Float -> Picture
outerEdge gs =
    let wallLength = windowSize / gs in
    pictures [
        Line [(x0, y0), (xMax, y0)],
        Line [(xMax, y0), (xMax, yMax + wallLength)],
        Line [(x0, yMax), (xMax, yMax)],
        Line [(x0, y0 - wallLength), (x0, yMax)]
    ]


{-func
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
-}
render :: GameState -> Picture
render game
    | startMenu game =
        pictures [
            --scale 0.2 0.2 $ (testImage game),
            translate (-350) 100 $ scale 0.4 0.4 $ Text "Lets play a mazing game!",
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to never sleep again"
        ]
    | goalMenu game = 
        pictures [
            translate (-250) 100 $ scale 0.4 0.4 $ Text ("You passed level " ++ show (playerLevel game) ++ "!"),
            translate (-250) 50 $ scale 0.4 0.4 $ Text ("With " ++show (steps game)++" moves"),
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to go to the next level",
            translate (x0) (yMax) $ scale 0.4 0.4 $ text (show (diffUTCTime  (timeNow game) (startTime game)))
        ]
        
    | otherwise = 
        pictures [
            outerEdge (gridSize game),
            mazePicture game,
            translate (x0) (yMax+10) $ scale 0.4 0.4 $ text (show (diffUTCTime  (timeNow game) (startTime game))),
            
            uncurry translate (gridCalculate (goalCoords game) (gridSize game)) $ scale 0.2 0.2 $ (testImageG game),--color green $ circleSolid ((windowSize / gridSize game) / 2 * 0.6),
            uncurry translate (gridCalculate (playerCoords game) (gridSize game)) $  scale 0.5 0.5 $ (testImageP game),
            
            translate (-200) (100) $ scale 0.4 0.4 $ text (show (playerCoords game)),
            translate (-200) 0 $ scale 0.4 0.4 $ text (show (goalCoords game)),
            translate (x0) (y0+10) $ scale 0.4 0.4 $ text ("Steps: "++show (steps game)),
            translate (300) (y0+10) $ scale 0.4 0.4 $ Text ("Level: "++show (playerLevel game))
        ]