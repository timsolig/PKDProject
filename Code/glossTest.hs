import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

windowWidth = 200
windowHeight = 200
x0 = -windowWidth/2
y0 = -windowHeight/2
xMax = negate x0
yMax = negate y0

rows = 5 :: Float
cols = 5 :: Float

window :: Display
window = InWindow "Nice Window" (200, 200) (50, 50)

background :: Color
background = white

-- Maze edges
edges = [
    Line [(x0, y0),(xMax, y0)],
    Line [(x0, yMax), (xMax, yMax)],
    Line [(x0, y0), (x0, yMax)],
    Line [(xMax, y0), (xMax, yMax)]
    ]

addColumns :: Float -> Float -> [Picture]
addColumns num tot
    | num > 0 = Line [(x0 + num * windowWidth/tot,y0), (x0 + num * windowWidth/tot,yMax)] : addColumns (num-1) tot
    | otherwise = []

addRows :: Float -> Float -> [Picture]
addRows num tot
    | num > 0 = Line [(x0, y0 + num * windowHeight/tot), (xMax, y0 + num * windowHeight/tot)] : addRows (num-1) tot
    | otherwise = []

drawing :: Picture
drawing = pictures ((addColumns (cols-1) cols) ++ (addRows (rows-1) rows))

main :: IO ()
main = display window background drawing