import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

--- INSTÄLLNINGAR Start ---

-- Fönsterstorlek
windowSize = 500

-- Antal rutor
gridSize = 3

-- Koordinater för väggar
wallList = [
    (0,1),
    (2,0),
    (2,1),
    (1,1)
    ]

--- INSTÄLLNINGAR Slut ---

xMax = windowSize / 2
x0 = negate xMax
y0 = windowSize / 2
yMax = negate y0
cellSize = windowSize / gridSize

window :: Display
window = InWindow "A Mazing Game" (round windowSize, round windowSize) (0,0)

background :: Color
background = white

cellMaker :: Float -> Float -> Picture
cellMaker x y
    | not (elem ((x-1), (y-1)) wallList) = Blank
    | otherwise =
        Polygon [
            (x0 + x * cellSize, y0 - y * cellSize),
            (x0 + x * cellSize, y0 - (y-1) * cellSize),
            (x0 + (x-1) * cellSize, y0 - (y-1) * cellSize),
            (x0 + (x-1) * cellSize, y0 - y * cellSize)
        ]

rowMaker :: Float -> Float -> [Picture]
rowMaker 0 _ = []
rowMaker _ 0 = []
rowMaker x y =
    cellMaker x y : rowMaker (x-1) y

gridMaker :: Float -> [Picture]
gridMaker 0 = []
gridMaker n = rowMaker gridSize n ++ gridMaker (n-1)

drawing :: Picture
drawing = pictures (gridMaker gridSize)

main :: IO ()
main = display window background drawing