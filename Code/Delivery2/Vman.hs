module Main (main) where

import Graphs
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Time
import System.IO.Unsafe

windowSize, xMax, x0, yMax, y0 :: Float
windowSize = 1000
xMax = windowSize / 2
x0 = negate $ windowSize / 2
y0 = windowSize / 2
yMax = negate $ windowSize / 2

window :: Display
window = InWindow "Rubrik" (round windowSize + 200, round windowSize + 200) (10, 10)

background :: Color
background = white

fps :: Int
fps = 30

{-GameState 
  Represents the 


  INVARIANT: gridSize > 0
-}
data GameState = Game {
        startMenu    :: Bool,
        goalMenu     :: Bool,
        gridSize     :: Float,
        mazePicture  :: Picture,
        walls        :: Maze,
        playerCoords :: (Float, Float),
        playerLevel  :: Int,
        goalCoords   :: (Float, Float),
        steps        :: Int,
        testImageP   :: Picture,
        testImageG   :: Picture,
        seconds      :: Float
    }

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
    RETURNS:
    EXAMPLES:
-}
drawWalls :: Maze -> Float -> [Picture]
drawWalls [] _ = []
-- drawWalls lst@(x:xs) gridSize = foldl (\x -> singleWall x gridSize) Blank lst
drawWalls (x:xs) gridSize =
    singleWall x gridSize : drawWalls xs gridSize
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

{- createMaze size
  Creates the walls of a maze.
    PRE: size > 0
    RETURNS: 
    EXAMPLES:
-}
createMaze :: Float -> Maze
createMaze gridSize = Graphs.getMaze gridSize


{-gridCalculate
  Jag förstår inte den här geometrin, kan någon pls tala om för mig
    PRE:
    RETURNS:
    EXAMPLES:
-}
gridCalculate :: (Float, Float) -> Float -> (Float, Float)
gridCalculate (x, y) gs = 
    let wallLength = windowSize / gs
    in (x0 + (x + 0.5) * wallLength, y0 - (y + 0.5) * wallLength)



{-outerEdge size
  Creates picture of the outer edge of a maze.
    PRE: n >= 0
    RETURNS: A square whose side has length "size".
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

{-render state
  Creates the picture of a game state
    RETURNS: The picture of the maze or menu which 'state' represents.
-}
render :: GameState -> Picture
render game
    | startMenu game =
        pictures [
            --scale 0.2 0.2 $ (testImage game),
            translate (-350) 100 $ scale 0.4 0.4 $ Text "Lets play a mazeing game!",
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to never sleep again"
        ]

    | goalMenu game = 
        pictures [
            translate (-250) 80 $ scale 0.4 0.4 $ Text ("You passed level " ++ show (playerLevel game) ++ "!"),
            translate (-320) 0 $ scale 0.3 0.3 $ Text ("With " ++ show (steps game) ++ " moves in " ++ (show (round (seconds game))) ++ " seconds"),
            translate (-250) (-60) $ scale 0.2 0.2 $ Text "Press [space] to go to the next level"
        ]
            --translate (-250) (-60) $ scale 0.4 0.4 $ Text ("Time: "++(show (showTimeDiff  (timeNow game) (startTime game))))
        
    | otherwise = 
        pictures [
            outerEdge (gridSize game),
            mazePicture game,
            translate (-150) (y0 + 10) $ scale 0.4 0.4 $ Text ("Time: " ++ show (round (seconds game))),
            uncurry translate (gridCalculate (goalCoords game) (gridSize game)) $ scale 0.3 0.3 $ (testImageG game),--color green $ circleSolid ((windowSize / gridSize game) / 2 * 0.6),
            uncurry translate (gridCalculate (playerCoords game) (gridSize game)) $ scale 0.4 0.4 $ (testImageP game),
            translate x0 (y0 + 10) $ scale 0.4 0.4 $ Text ("Steps: "++show (steps game)),
            translate 300 (y0 + 10) $ scale 0.4 0.4 $ Text ("Level: "++show (playerLevel game))
        ]



{-handleKeys keyStroke state
  Updates the state depending on inpute from the player.
    RETURNS: The state of the game according to "keyStroks" i.e. the key pressed by the player.
-}
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey key) Down _ _) game
    | startMenu game && key == KeySpace = 
        let 
            newGridSize = 10
            wallLength = windowSize / newGridSize
            newWalls = createMaze newGridSize
        in
            Game {
                startMenu    = False,
                goalMenu     = False,
                gridSize     = newGridSize,
                mazePicture  = wallPicture newWalls newGridSize,
                walls        = newWalls,
                playerCoords = (-1, 0),
                playerLevel  = 1,
                goalCoords   = (newGridSize, newGridSize - 1),
                steps        = 0,
                testImageP   = testImageP game,
                testImageG   = testImageG game,
                seconds      = 0
            }
    | goalMenu game && key == KeySpace =
        let 
            newGridSize = gridSize game + 5
            wallLength = windowSize / newGridSize
            newWalls = createMaze newGridSize
        in
            Game {
                startMenu    = False,
                goalMenu     = False,
                gridSize     = newGridSize,
                mazePicture  = wallPicture newWalls newGridSize,
                walls        = newWalls,
                playerCoords = (-1, 0),
                playerLevel  = playerLevel game + 1,
                goalCoords   = (newGridSize, newGridSize - 1),
                steps        = 0,
                testImageP   = testImageP game,
                testImageG   = testImageG game,
                seconds      = 0
            }
    | not (startMenu game || goalMenu game) =
        let
            (x, y) = playerCoords game
            wallLength = windowSize / (gridSize game)
            direction = case key of
                        KeyUp    -> (x, y - 1)
                        KeyRight -> (x + 1, y)
                        KeyDown  -> (x, y + 1)
                        KeyLeft  -> (x - 1, y)
                        _        -> (x, y)
            newPlayerCoords = 
                if validMove (x,y) direction (gridSize game) (walls game) then direction
                else (x, y)
            newSteps = if validMove (x,y) direction (gridSize game) (walls game) then ((steps game)+1)
                else steps game
            goalMenuStatus =
                if newPlayerCoords == goalCoords game then True
                else False               
        in
            Game {
                startMenu    = False,
                goalMenu     = goalMenuStatus,
                gridSize     = gridSize game,
                mazePicture  = mazePicture game,
                walls        = walls game,
                playerCoords = newPlayerCoords,
                playerLevel  = playerLevel game,
                goalCoords   = goalCoords game,
                steps        = newSteps,
                testImageP   = testImageP game,
                testImageG   = testImageG game,
                seconds      = seconds game
            }
handleKeys _ game = game


{- validMove cell1 cell2 gridSize maze
  Checks whether a player move is valid in a certain move.
    RETURNS: If there is no wall between 'cell1' and 'cell2' in the maze 'maze' (which has size 'gridSize') then True otherwise False. 
    EXAMPLES:
-}
validMove :: Cell -> Cell -> Float -> Maze -> Bool
validMove (-1, 0) (0, 0) _ _ = True
validMove currentPath targetPath@(x, y) gs walls =
    x == gs && y == gs - 1 ||
    not (
        x < 0 ||
        x >= gs ||
        y < 0 ||
        y >= gs ||
        elem (currentPath, targetPath) walls ||
        elem (targetPath, currentPath) walls
        )


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

main :: IO ()
main = do
    --Loads pictures
    p1 <- loadBMP "1.bmp"
    p2 <- loadBMP "2.bmp"
    play window background fps (initialState p1 p2) render handleKeys counter