module Main (main) where

import Graphs
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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

data GameState = Game {
        startMenu    :: Bool,
        goalMenu     :: Bool,
        gridSize     :: Float,
        mazePicture  :: Picture,
        walls        :: Graphs.Maze,
        playerCoords :: (Float, Float),
        playerLevel  :: Int,
        goalCoords   :: (Float, Float)
    }
    
initialState :: GameState
initialState = Game {
        startMenu    = True,
        goalMenu     = False,
        gridSize     = undefined,
        mazePicture  = undefined,
        walls        = undefined,
        playerCoords = undefined,
        playerLevel  = undefined,
        goalCoords   = undefined
    }

scoreList :: Picture
scoreList = pictures [
        translate (-170) (-150) $ scale 0.2 0.2 $ Text "1. Janne 5'000 points",
        translate (-170) (-190) $ scale 0.2 0.2 $ Text "2. Goran 5 points",
        translate (-170) (-230) $ scale 0.2 0.2 $ Text "3. Big boy -54 points"
    ]

createWalls :: Graphs.Maze -> Float -> [Picture]
createWalls [] _ = []
createWalls (x:xs) gridSize =
    singleWall x gridSize : createWalls xs gridSize
  where
    singleWall :: Graphs.Wall -> Float -> Picture
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


createWall :: Float -> Graphs.Maze
createWall gridSize = Graphs.iterDFS $ Graphs.createCells gridSize

wallPicture :: Graphs.Maze -> Float -> Picture
wallPicture wall gridSize = Pictures (createWalls wall gridSize)

gridCalculate :: (Float, Float) -> Float -> (Float, Float)
gridCalculate (x, y) gs = 
    let wallLength = windowSize / gs
    in (x0 + (x + 0.5) * wallLength, y0 - (y + 0.5) * wallLength)

outerEdge :: Float -> Picture
outerEdge gs =
    let wallLength = windowSize / gs in
    pictures [
        Line [(x0, y0), (xMax, y0)],
        Line [(xMax, y0), (xMax, yMax + wallLength)],
        Line [(x0, yMax), (xMax, yMax)],
        Line [(x0, y0 - wallLength), (x0, yMax)]
    ]

render :: GameState -> Picture
render game
    | startMenu game =
        pictures [
            translate (-350) 100 $ scale 0.4 0.4 $ Text "Lets play a mazing game!",
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to never sleep again",
            translate (-170) (-100) $ scale 0.4 0.4 $ Text "Scoreboard",
            scoreList
        ]
    | goalMenu game = 
        pictures [
            translate (-250) 100 $ scale 0.4 0.4 $ Text ("You passed level " ++ show (playerLevel game) ++ "!"),
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to go to the next level"
        ]
    | otherwise = 
        pictures [
            outerEdge (gridSize game),
            mazePicture game,
            uncurry translate (gridCalculate (goalCoords game) (gridSize game)) $ color green $ circleSolid ((windowSize / gridSize game) / 2 * 0.6),
            uncurry translate (gridCalculate (playerCoords game) (gridSize game)) $ color red $ circleSolid ((windowSize / gridSize game) / 2 * 0.8),
            translate (-200) (100) $ scale 0.4 0.4 $ text (show (playerCoords game)),
            translate (-200) 0 $ scale 0.4 0.4 $ text (show (goalCoords game))
        ]

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey key) Down _ _) game
    | startMenu game && key == KeySpace = 
        let 
            newGridSize = 10
            wallLength  = windowSize / newGridSize
            newWalls    = createWall newGridSize
        in
            Game {
                startMenu    = False,
                goalMenu     = False,
                gridSize     = newGridSize,
                mazePicture  = wallPicture newWalls newGridSize,
                walls        = newWalls,
                playerCoords = (-1, 0),
                playerLevel  = 1,
                goalCoords   = (newGridSize, newGridSize - 1)
            }
    | goalMenu game && key == KeySpace =
        let 
            newGridSize = gridSize game + 5
            wallLength  = windowSize / newGridSize
            newWalls    = createWall newGridSize
        in
            Game {
                startMenu    = False,
                goalMenu     = False,
                gridSize     = newGridSize,
                mazePicture  = wallPicture newWalls newGridSize,
                walls        = newWalls,
                playerCoords = (-1, 0),
                playerLevel  = playerLevel game + 1,
                goalCoords   = (newGridSize, newGridSize - 1)
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
                goalCoords   = goalCoords game
            }
handleKeys _ game = game

validMove :: (Float, Float) -> (Float, Float) -> Float -> Graphs.Maze -> Bool
validMove (-1, 0) (0, 0) _ _ = True
validMove currentPath@(cx, cy) targetPath@(tx,ty) gs walls =
    tx == gs && ty == gs - 1 ||
    not (
        tx < 0 ||
        tx >= gs ||
        ty < 0 ||
        ty >= gs ||
        elem (currentPath, targetPath) walls ||
        elem (targetPath, currentPath) walls
    )

main :: IO ()
main = play window background fps initialState render handleKeys (const id)