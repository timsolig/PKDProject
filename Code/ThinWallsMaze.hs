import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

{-
Skapar en maze med "tunna väggar"

-}

--- INSTÄLLNINGAR Start ---

-- Fönsterstorlek
windowSize = 500

-- Antal rutor
gridSize = 3

-- Koordinater för väggar
walls = [
    ((2,0), (2,1)),
    ((2,0), (1,0))
    ]

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

createWall :: ((Float, Float), (Float, Float)) -> Picture
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

createWalls :: [((Float, Float), (Float, Float))] -> [Picture]
createWalls [] = []
createWalls (x:xs) =
    createWall x : createWalls xs

drawing :: Picture
drawing = pictures (createWalls walls)

main :: IO ()
main = display window background drawing