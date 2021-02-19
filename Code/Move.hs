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
size = 20

--- INSTÄLLNINGAR Slut ---

--Initial world
type World = (Float,Float)
initialWorld = drawing  

-- gridSize :: Float
gridSize = size

-- walls = Graphs.prim cells, koordinater för väggar
walls = Graphs.iterativeDFS $ Graphs.createCells size

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

--Hur stor spelaren ska vara  
player:: Picture
player = (color red (circleSolid x))
  where x = windowSize / (2*gridSize)

-- Flyttar player med translate
-- Starta längst upp i vänster hörn = x0 yx
-- Varje steg som tas så ska wallLength gås
movePlayer ::Float -> Float -> Picture
movePlayer x y = Pictures [text cord, translate (x*wallLength+ wallRadius) (y*wallLength-wallRadius){-(x0+x*wallLength + wallRadius) (y0+y*wallLength - wallRadius) -}(player),initialWorld]
  where cord = show x ++","++ show y
windowDisplay :: Display
windowDisplay = InWindow "Testar Gloss" (round windowSize, round windowSize) (10, 10)

-- Laddar om frame med samma maze med player på ny eller samma position
drawPlayfield :: World -> Picture
drawPlayfield (x,y) = translate 0 0 $ movePlayer x y
 
 -- where world = 
 --                     Pictures [color black (line [ (-100, -300), (-100,  300) ]) <>
 --                               color black (line [ ( 100, -300), ( 100,  300) ]) <>
 --                               color black (line [ (-300,  100), ( 300,  100) ]) <>
 --                               color black (line [ (-300, -100), ( 300, -100) ])]
--
--drawingFunc :: World -> Picture
--drawingFunc (x, y) = translate x y (ThickCircle 20 20)

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey key) Down _ _ ) (x,y) = case validMove (x,y) direction of 
                                                                                      True -> direction
                                                                                      _    -> (x,y)
  where direction = case key of 
                              KeyUp -> (x, y + 1)
                              KeyDown -> (x, y - 1)
                              KeyRight -> (x + 1, y)
                              KeyLeft -> (x - 1, y)
inputHandler _ (x,y) = (x,y)

validMove :: Main.Cell -> Main.Cell -> Bool
validMove a b = not $ (a,b) `elem` walls || (b,a) `elem` walls


updateFunc :: Float -> World -> World
updateFunc _ (x, y) = (x, y)
  

main :: IO ()
main = play
  windowDisplay -- size of window
  background --color
  30 --fps
  (0,0) --Initial World
  drawPlayfield
  inputHandler
  updateFunc