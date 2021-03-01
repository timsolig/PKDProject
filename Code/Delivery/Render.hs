module Render where

import Graphs

import GameData

import Graphics.Gloss

{- The size of the Gloss window -}
windowSize = 1000 :: Float

{-Information about Gloss Window | Window title ()-}
window = InWindow "A Maz(e)ing game" (round windowSize + 200, round windowSize + 200) (10, 10) :: Display

{- Background colour -}
background = white :: Color

{- X-coordinate of the cell in the upper left corner -}
x0 = negate $ windowSize / 2 :: Float

{- Y-coordinate of the cell in the upper left corner -}
y0 = windowSize / 2 :: Float

{- X-coordinate of the cell in the lower right corner -}
xMax = windowSize / 2 :: Float

{- Y-coordinate of the cell in the lower right corner -}
yMax = negate $ windowSize / 2 :: Float

{- createMaze size
  Creates the walls of a maze.
    PRE: size > 0
    RETURNS: 
    EXAMPLES:
-}
createMaze :: Float -> Maze
createMaze gridSize = Graphs.getMaze gridSize


{-wallPicture maze 
  Creates the picture only containing the walls of a maze.
    RETURNS:
    EXAMPLES:
-}
wallPicture :: Maze -> Float -> Picture
wallPicture wall gridSize = Pictures (drawWalls wall gridSize)


{-drawWalls maze size 
  Draws the walls of a maze.
    PRE: size > 0
    RETURNS: Pictures of walls in "maze"
-}
drawWalls :: Maze -> Float -> [Picture]
drawWalls [] _ = []
drawWalls lst size = map (singleWall size) lst
    where
        {- singleWall size wallCoord 
             Draws the wall between two coordinates.
               PRE: size > 0
               RETURNS: Picture of a wall with length based on "size" laying between coordinates in "wallCoord".
        -}
        singleWall :: Float -> Wall -> Picture
        singleWall gridSize ((c1_x, c1_y), (c2_x, c2_y)) =
            Line [(l1_x, l1_y), (l2_x, l2_y)]
            where
                wallLength = windowSize / gridSize
                x_mid = x0 + wallLength * max c1_x c2_x
                y_mid = y0 - wallLength * max c1_y c2_y
                (l1_x, l2_x, l1_y, l2_y) =
                    if (c1_x == c2_x) 
                        then (x_mid, x_mid + wallLength, y_mid, y_mid)
                    else (x_mid, x_mid, y_mid, y_mid - wallLength)


{-translateCoordinates cell gridSize
    Translates from cartesian coordinates to Gloss Window coordinates. 
      PRE: gridSize > 0
      RETURNS: The coordinates of "cell" in the gloss window.
      EXAMPLES: 

        translateCoordinates (0, 0) 25 == (-480.0, 480.0)
        
        translateCoordinates (1, 2) 20 == (-425.0, 375.0)

-}
translateCoordinates :: (Float, Float) -> Float -> (Float, Float)
translateCoordinates (x, y) gridSize = 
    let wallLength = windowSize / gridSize
    in (x0 + (x + 0.5) * wallLength, y0 - (y + 0.5) * wallLength)


{-outerEdge size
  Creates picture of the outer edge of a grid.
    PRE: size > 0
    RETURNS: A square whose side has length "size".
-}
outerEdge :: Float -> Picture
outerEdge gridSize =
    let wallLength = windowSize / gridSize in
    pictures [
        Line [(x0, y0), (xMax, y0)],
        Line [(xMax, y0), (xMax, yMax + wallLength)],
        Line [(x0, yMax), (xMax, yMax)],
        Line [(x0, y0 - wallLength), (x0, yMax)]
    ]


{-render state
  Creates the picture of a game state.
    RETURNS: The picture of the maze or menu which 'state' represents.

-}
render :: GameState -> Picture
render game
    | startMenu game =
        pictures [
            swagLines,

            color white $ Polygon [(-400, 200), (400, 200), (400, -70), (-400, -70)],

            translate (-360) 100 $ scale 0.4 0.4 $ Text "Lets play a mazeing game!",
            
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to never sleep again"

            
        ]

    | goalMenu game = 
        pictures [
            translate (-250) 80 $ scale 0.4 0.4 $ Text ("You passed level " ++ show (playerLevel game) ++ "!"),
            
            translate (-320) 0 $ scale 0.3 0.3 $ Text ("With " ++ show (steps game) ++ " moves in " ++ show (round (seconds game)) ++ " seconds"),
            
            translate (-250) (-60) $ scale 0.2 0.2 $ Text "Press [space] to go to the next level"
        ]
            
    | otherwise = 
        pictures [
            outerEdge (gridSize game),
            
            mazePicture game,
            
            translate (-150) (y0 + 10) $ scale 0.4 0.4 $ Text ("Time: " ++ show (round (seconds game))),
            
            uncurry translate (translateCoordinates (goalCoords game) (gridSize game)) $ color green $ scale 0.6 0.6 $ circleSolid 40, --scale (0.2 * (gridSize game)) (0.2 * gridSize game) $ testImageG game,
            
            uncurry translate (translateCoordinates (playerCoords game) (gridSize game)) $ color red $ scale 0.6 0.6 $ circleSolid 40,--scale 0.5 0.5 $ testImageP game,
            
            translate x0 (y0 + 10) $ scale 0.4 0.4 $ Text ("Steps: " ++show (steps game)),
            
            translate 300 (y0 + 10) $ scale 0.4 0.4 $ Text ("Level: " ++show (playerLevel game))
        ]



counter :: Float -> GameState -> GameState
counter sec game
    | not (startMenu game || goalMenu game) =
        Game {
            startMenu    = startMenu game,
            goalMenu     = goalMenu game,
            gridSize     = gridSize game,
            mazePicture  = mazePicture game,
            walls        = walls game,
            playerCoords = playerCoords game,
            playerLevel  = playerLevel game,
            goalCoords   = goalCoords game,
            steps        = steps game,
            testImageP   = testImageP game,
            testImageG   = testImageG game,
            seconds      = seconds game + sec
        }
    | otherwise = game
