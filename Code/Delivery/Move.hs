module Move where

import Render

import GameData

import Graphs

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


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
                if validMove (x,y) direction (gridSize game) (walls game) 
                    then direction
                else (x, y)
            
            newSteps = 
                if validMove (x,y) direction (gridSize game) (walls game) 
                    then (steps game) + 1
                else steps game
            
            goalMenuStatus =
                if newPlayerCoords == goalCoords game 
                    then True
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


