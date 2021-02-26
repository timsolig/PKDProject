module Move where

import Render
import Data.Time
import Graphics.Gloss
import Graphs 
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe


getTime :: a -> UTCTime
getTime _ =  unsafeDupablePerformIO getCurrentTime

{-validMove
  text about func
    PRE:
    RETURNS:
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
  text about func
    PRE:
    RETURNS:
    EXAMPLES:
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
                startTime    = getTime playerLevel,
                timeNow      = getTime playerLevel
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
                startTime    = getTime playerLevel,
                timeNow      = getTime playerLevel
            }
    | otherwise =
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
                startTime    = startTime game,
                timeNow      = getTime playerLevel
                }
handleKeys _ game = game