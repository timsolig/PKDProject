import Graphs
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

{- Skapar en maze med "tunna väggar" -}

type Cell = (Float,Float)
--- INSTÄLLNINGAR Start ---

-- Fönsterstorlek
windowSize = 1000   

-- Antal rutor
size :: Float
size = 50.0

--- INSTÄLLNINGAR Slut ---

--Initial world
type World = (Float,Float)
initialWorld = drawing  

-- gridSize :: Float
gridSize = size

-- walls = Graphs.prim cells, koordinater för väggar
walls = Graphs.iterDFS $ Graphs.createCells size

-- till move func
moveDist = windowSize / gridSize
startPosFix = moveDist/2

xMax = windowSize / 2
x0 = negate xMax
y0 = windowSize / 2
yMax = negate y0

wallLength = windowSize / gridSize
wallRadius = wallLength / 2

window :: Display   
window = InWindow "A Mazing Game" (round windowSize, round windowSize) (0,0)

background :: Color
background = white

createWall :: (Main.Cell, Main.Cell) -> Picture
createWall ((c1_x, c1_y), (c2_x, c2_y)) =
    Line [(l1_x, l1_y), (l2_x, l2_y)]
    where
        x_mid = x0 + wallLength * max c1_x c2_x
        y_mid = y0 - wallLength * max c1_y c2_y
        (l1_x, l2_x, l1_y, l2_y) =
            if (c1_x == c2_x) then
                (x_mid, x_mid + wallLength, y_mid, y_mid)
            else
                (x_mid, x_mid, y_mid, y_mid - wallLength)

createWalls' :: [(Main.Cell, Main.Cell)] -> [Picture]
createWalls' [] = []
createWalls' (x:xs) =
    createWall x : createWalls' xs

drawing :: Picture
drawing = pictures (createWalls' (walls :: [(Main.Cell,Main.Cell)]))


--                      MOVE
-- =================================================
player:: Picture
player = (color red (circleSolid x))
  where x = windowSize / (2*gridSize)


movePlayer ::Float -> Float -> Picture
movePlayer x y= Pictures [translate (x) (y) (player),initialWorld]

windowDisplay :: Display
windowDisplay = InWindow "Testar Gloss" (1080, 1080) (10, 10)

-- Laddar om frame med samma maze med player på ny eller samma position
drawPlayfield :: World -> Picture
drawPlayfield (x,y) = translate 0 0 $ movePlayer x y
 
 -- where world = 
 --                     Pictures [color black (line [ (-100, -300), (-100,  300) ]) <>
 --                               color black (line [ ( 100, -300), ( 100,  300) ]) <>
 --                               color black (line [ (-300,  100), ( 300,  100) ]) <>
 --                               color black (line [ (-300, -100), ( 300, -100) ])]

drawingFunc :: World -> Picture
drawingFunc (x, y) = translate x y (ThickCircle 20 20)

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = if (y+moveDist) <= (windowSize/2) then (x, y + moveDist) else (x,y)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = if (y-moveDist) >= (-windowSize/2) then (x, y - moveDist)  else (x,y)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = if (x+moveDist) <= (windowSize/2) then (x+moveDist, y) else (x,y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = if (x-moveDist) >= (-windowSize/2) then (x-moveDist, y)  else (x,y)
inputHandler _ w = w

updateFunc :: Float -> World -> World
updateFunc _ (x, y) = (x, y)
  

main :: IO ()
main = play
  windowDisplay -- size of window
  background --color
  30 --fps
  (startPosFix,startPosFix) --Initial World
  drawPlayfield
  inputHandler
  updateFunc



--
--main :: IO ()
--main = display window background drawing