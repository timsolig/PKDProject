module Main (main) where

import Graphs
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowSize = 1000 :: Float

window :: Display
window = InWindow "List" (500, 500) (10, 10)

background :: Color
background = white

fps :: Int
fps = 30

data GameState = Game {
        menuActive :: Bool,
        onGoal :: Bool,
        playerCoords :: (Float, Float),
        playerLevel :: Int,
        gridSize :: Float,
        walls :: Picture,
        wallLength :: Float,
        wallRadius :: Float,
        xMax :: Float,
        x0 :: Float,
        y0 :: Float,
        yMax :: Float,
        goalCoords :: (Float, Float)
    }

initialState :: GameState 
initialState = Game {
        menuActive = True,
        onGoal = False,
        playerCoords = (negate (windowSize / 2) + windowSize / 20, windowSize / 2 - windowSize / 20),
        playerLevel = 1,
        gridSize = 10,
        walls = (wallPicture (negate (windowSize / 2), windowSize / 2, windowSize / 10, 10)),
        wallLength = windowSize / 10,
        wallRadius = windowSize / 20,
        xMax = windowSize / 2,
        x0 = negate (windowSize / 2),
        y0 = windowSize / 2,
        yMax = negate (windowSize / 2),
        goalCoords = (windowSize / 2 - windowSize / 20, negate (windowSize / 2) + windowSize / 20)
    }

scoreList :: Picture
scoreList = pictures [
        translate (-170) (-150) $ scale 0.2 0.2 $ Text "1. Janne 5'000 points",
        translate (-170) (-190) $ scale 0.2 0.2 $ Text "2. Goran 5 points",
        translate (-170) (-230) $ scale 0.2 0.2 $ Text "3. Big boy -54 points"
    ]

createWalls :: Graphs.Maze -> (Float, Float, Float, Float) -> [Picture]
createWalls [] _ = []
createWalls (x:xs) tpl@(x0, y0, wallLength, gridSize) =
    singleWall x (x0, y0, wallLength) : createWalls xs tpl
  where
    singleWall :: Graphs.Wall -> (Float, Float, Float) -> Picture
    singleWall ((c1_x, c1_y), (c2_x, c2_y)) (x0, y0, wallLength) =
        Line [(l1_x, l1_y), (l2_x, l2_y)]
        where
            x_mid = x0 + wallLength * max c1_x c2_x
            y_mid = y0 - wallLength * max c1_y c2_y
            (l1_x, l2_x, l1_y, l2_y) =
                if (c1_x == c2_x) then
                    (x_mid, x_mid + wallLength, y_mid, y_mid)
                else
                    (x_mid, x_mid, y_mid, y_mid - wallLength)

wallPicture :: (Float, Float, Float, Float) -> Picture
wallPicture tpl@(x0, y0, wallLength, gridSize) = Pictures (createWalls (Graphs.iterDFS $ Graphs.createCells 20) tpl)

render :: GameState -> Picture
render game
    | onGoal game = 
        pictures [
            translate (-250) 100 $ scale 0.4 0.4 $ Text ("You passed level " ++ show (playerLevel game - 1) ++ "!"),
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to go to the next level"
        ]
    | menuActive game =
        pictures [
            translate (-350) 100 $ scale 0.4 0.4 $ Text "Lets play a mazing game!",
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to never sleep again",
            translate (-170) (-100) $ scale 0.4 0.4 $ Text "Scoreboard",
            scoreList
        ]
    | otherwise =
        pictures [
            walls game,
            uncurry translate (playerCoords game) $ color red $ circleSolid 25
        ]

--handleKeys :: Event -> GameState -> GameState
--handleKeys (EventKey (SpecialKey key) Down _ _) game@(Game menuActive onGoal playerCoords@(x, y) playerLevel gridSize walls wallLength wallRadius xMax x0 y0 yMax goalCoords)
--    | (menuActive || onGoal) =
--        if key == KeySpace then
--            Game {
--                menuActive = False,
--                onGoal = False,
--                playerCoords = playerCoords,
--                playerLevel = playerLevel,
--                gridSize = gridSize,
--                walls = walls,
--                wallLength = wallLength,
--                wallRadius = wallRadius,
--                xMax = xMax,
--                x0 = x0,
--                y0 = y0,
--                yMax = yMax,
--                goalCoords = goalCoords
--            }
--        else game
--    | playerCoords == goalCoords = 
--        Game {
--            menuActive = False,
--            onGoal = True,
--            playerCoords = (negate (windowSize / 2) + windowSize / 20, windowSize / 2 - windowSize / 20),
--            playerLevel = playerLevel + 1,
--            gridSize = 15,
--            walls = (wallPicture (negate (windowSize / 2), windowSize / 2, windowSize / 10, 15)),
--            wallLength = windowSize / 10,
--            wallRadius = windowSize / 20,
--            xMax = windowSize / 2,
--            x0 = negate (windowSize / 2),
--            y0 = windowSize / 2,
--            yMax = negate (windowSize / 2),
--            goalCoords = (windowSize / 2 - wallRadius, negate (windowSize / 2) + wallRadius)
--        }
--    | otherwise = 
--        Game {
--            menuActive = False,
--            onGoal = False,
--            playerCoords = newCoords,
--            playerLevel = playerLevel,
--            gridSize = gridSize,
--            walls = walls,
--            wallLength = wallLength,
--            wallRadius = wallRadius,
--            xMax = xMax,
--            x0 = x0,
--            y0 = y0,
--            yMax = yMax,
--            goalCoords = goalCoords
--        }
--        where
--            newCoords = case key of
--                KeyUp    -> (x, y + wallLength)
--                KeyRight -> (x + wallLength, y)
--                KeyDown  -> (x, y - wallLength)
--                KeyLeft  -> (x - wallLength, y)
--handleKeys _ game = game

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey key) Down _ _) game@(Game menuActive onGoal playerCoords@(x, y) playerLevel gridSize walls wallLength wallRadius xMax x0 y0 yMax goalCoords) =
    if (menuActive || onGoal) then
        if key == KeySpace then
            Game {
                menuActive = False,
                onGoal = False,
                playerCoords = playerCoords,
                playerLevel = playerLevel,
                gridSize = gridSize,
                walls = walls,
                wallLength = wallLength,
                wallRadius = wallRadius,
                xMax = xMax,
                x0 = x0,
                y0 = y0,
                yMax = yMax,
                goalCoords = goalCoords
            }
        else game
    else
        if playerCoords == goalCoords then
            Game {
                menuActive = False,
                onGoal = True,
                playerCoords = (negate (windowSize / 2) + windowSize / 20, windowSize / 2 - windowSize / 20),
                playerLevel = playerLevel + 1,
                gridSize = gridSize + 5,
                walls = (wallPicture (negate (windowSize / 2), windowSize / 2, windowSize / 10, gridSize + 5)),
                wallLength = windowSize / gridSize + 5,
                wallRadius = windowSize / 20,
                xMax = windowSize / 2,
                x0 = negate (windowSize / 2),
                y0 = windowSize / 2,
                yMax = negate (windowSize / 2),
                goalCoords = (windowSize / 2 - wallRadius, negate (windowSize / 2) + wallRadius)
            }
        else
            Game {
                menuActive = False,
                onGoal = False,
                playerCoords = newCoords,
                playerLevel = playerLevel,
                gridSize = gridSize,
                walls = walls,
                wallLength = wallLength,
                wallRadius = wallRadius,
                xMax = xMax,
                x0 = x0,
                y0 = y0,
                yMax = yMax,
                goalCoords = goalCoords
            }
            where
                newCoords = case key of
                    KeyUp    -> (x, y + wallLength)
                    KeyRight -> (x + wallLength, y)
                    KeyDown  -> (x, y - wallLength)
                    KeyLeft  -> (x - wallLength, y)
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys (const id)