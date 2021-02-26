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

data GameState = Game {
        startMenu :: Bool,
        goalMenu :: Bool,
        gridSize :: Float,
        mazePicture :: Picture,
        walls :: Graphs.Maze,
        playerCoords :: (Float, Float),
        playerLevel :: Int,
        goalCoords :: (Float, Float),
        steps :: Int,
        testImageP :: Picture,
        testImageG :: Picture,
        startTime :: UTCTime,
        timeNow :: UTCTime
    }

getTime :: a -> UTCTime
getTime _ =  unsafeDupablePerformIO getCurrentTime

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

render :: GameState -> Picture
render game
    | startMenu game =
        pictures [
            --scale 0.2 0.2 $ (testImage game),
            translate (-350) 100 $ scale 0.4 0.4 $ Text "Lets play a mazing game!",
            translate (-250) 0 $ scale 0.2 0.2 $ Text "Press [space] to never sleep again",
            translate (-170) (-100) $ scale 0.4 0.4 $ Text "Scoreboard",
            scoreList
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
            translate (x0) (yMax) $ scale 0.4 0.4 $ text (show (diffUTCTime  (timeNow game) (startTime game))),
            
            uncurry translate (gridCalculate (goalCoords game) (gridSize game)) $ scale 0.2 0.2 $ (testImageG game),--color green $ circleSolid ((windowSize / gridSize game) / 2 * 0.6),
            uncurry translate (gridCalculate (playerCoords game) (gridSize game)) $  scale 0.5 0.5 $ (testImageP game),
            
            translate (-200) (100) $ scale 0.4 0.4 $ text (show (playerCoords game)),
            translate (-200) 0 $ scale 0.4 0.4 $ text (show (goalCoords game)),
            translate (x0) (y0+10) $ scale 0.4 0.4 $ text ("Steps: "++show (steps game)),
            translate (300) (y0+10) $ scale 0.4 0.4 $ Text ("Level: "++show (playerLevel game))
        ]

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey key) Down _ _) game
    | startMenu game && key == KeySpace = 
        let 
            newGridSize = 10
            wallLength = windowSize / newGridSize
            newWalls = createWall newGridSize
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
            newWalls = createWall newGridSize
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
            
        in
            let
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


--hittat en bugg. kan gå rakt ned genom väggen om måölet är till höger.
validMove :: (Float, Float) -> (Float, Float) -> Float -> Graphs.Maze -> Bool
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
        startTime    = getTime playerLevel,
        timeNow      = getTime playerLevel
    }

main :: IO ()
main = do
    p1 <- loadBMP "1.bmp"
    p2 <- loadBMP "2.bmp"
    play window background fps (initialState p1 p2) render handleKeys (const id)