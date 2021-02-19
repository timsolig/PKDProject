import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Graphs
{-
Skapar en maze med "tunna väggar"

-}

type Cell = (Float,Float)
--- INSTÄLLNINGAR Start ---

-- Fönsterstorlek
windowSize = 1000

-- Antal rutor

-- Koordinater för väggar
<<<<<<< HEAD
walls = [
    ((2,0), (2,1)),
    ((2,0), (1,0))
    ]
=======
size :: Float
<<<<<<< HEAD
size = 50.0
=======
size = 20.0
>>>>>>> 62e38a84dfc2ba88acd5de81db0bea0a4ce84ef4
>>>>>>> aea2adbcd3e312d46be230c06ab5f09138ee84e2

-- walls = Graphs.prim cells
walls = Graphs.iterDFS $ Graphs.createCells size

-- gridSize :: Float
gridSize = size
--- INSTÄLLNINGAR Slut ---

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

main :: IO ()
main = display window background drawing