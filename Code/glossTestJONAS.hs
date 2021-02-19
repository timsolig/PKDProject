
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Graphics.Gloss.Interface.Pure.Game

--import Graphics.Gloss.Data
--import Graphics.Gloss.Data.SpecialKey

type World = (Float,Float)

background :: Color
background = white

windowWidth = 1000
windowHeight = 1000
x0 = -windowWidth/2
y0 = -windowHeight/2
xMax = negate x0
yMax = negate y0

rows = 100 :: Float
cols = 100 :: Float

moveDist = windowWidth / rows
startPosFix = moveDist/2

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
drawing = pictures ((addColumns (cols-1) cols) ++ (addRows (rows-1) rows)++edges)

initialWorld = drawing        



main :: IO ()
main = play
  windowDisplay -- size of window
  background --color
  30 --fps
  (startPosFix,startPosFix) --Initial World
  drawPlayfield
  inputHandler
  updateFunc

player:: Picture
player = (color red (circleSolid x))
  where x = windowWidth / (2*rows)


hej ::Float -> Float -> Picture
hej x y= Pictures [translate (x) (y) (player),initialWorld]


--
--hej ::Float -> Float -> Picture
--hej x y= Pictures [color black (line [ (-100, -300), (-100,  300) ]) <>
--                color black (line [ ( 100, -300), ( 100,  300) ]) <>
--                color black (line [ (-300,  100), ( 300,  100) ]) <>
--                color black (line [ (-300, -100), ( 300, -100) ]) <>
--                translate x y (color red (circleSolid 100))]


windowDisplay :: Display
windowDisplay = InWindow "Testar Gloss" (1080, 1080) (10, 10)


drawPlayfield :: World -> Picture
drawPlayfield (x,y) = translate 0 0 $ hej x y
 
 -- where world = 
 --                     Pictures [color black (line [ (-100, -300), (-100,  300) ]) <>
 --                               color black (line [ ( 100, -300), ( 100,  300) ]) <>
 --                               color black (line [ (-300,  100), ( 300,  100) ]) <>
 --                               color black (line [ (-300, -100), ( 300, -100) ])]

drawingFunc :: World -> Picture
drawingFunc (x, y) = translate x y (ThickCircle 20 20)

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = if (y+moveDist) <= (windowHeight/2) then (x, y + moveDist) else (x,y)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = if (y-moveDist) >= (-windowHeight/2) then (x, y - moveDist)  else (x,y)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = if (x+moveDist) <= (windowHeight/2) then (x+moveDist, y) else (x,y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = if (x-moveDist) >= (-windowHeight/2) then (x-moveDist, y)  else (x,y)
inputHandler _ w = w

updateFunc :: Float -> World -> World
updateFunc _ (x, y) = (x, y)
  
  


  --(towardCenter x, towardCenter y)
  --where
  --  towardCenter :: Float -> Float
  --  towardCenter c = if abs c < 0.25
  --    then 0
  --    else if c > 0
  --      then c - 0.25
  --      else c + 0.25
--
--
--





--type Model = (Float,Float)
--
--main = simulate
--  FullScreen
--  white
--  simulationRate
--  initialModel
--  drawingFunc
--  updateFunc
--  where
--    simulationRate :: Int
--    simulationRate = 20
--
--    initialModel :: Model
--    initialModel = (0,0)
--
--    drawingFunc :: Model -> Picture
--    drawingFunc (theta, dtheta) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]
--
--
--    updateFunc :: ViewPort -> Float -> Model -> Model
--    updateFunc _ dt (theta, dtheta) = (theta + dt * dtheta, dtheta - dt * (cos theta))
--


--main :: IO ()
--main = play
--  windowDisplay
--  black
--  20
--  (0, 0)
--  drawingFunc
--  inputHandler
--  updateFunc
--
--windowDisplay :: Display
--windowDisplay = InWindow "Testar Gloss" (1080, 1080) (10, 10)
--
--main :: IO ()
--main = display windowDisplay white someFunc
--
--someFunc :: Picture
--someFunc = (Circle 89)
--

--playIOSource#
--
-- :: forall world. Display	<
--Display mode.
--
-- -> Color	
--Background color.
--
-- -> Int	
--Number of simulation steps to take for each second of real time.
--
-- -> world	
-- The initial world.
--
-- -> (world -> IO Picture)	
--An action to convert the world a picture.
--
-- -> (Event -> world -> IO world)	
--A function to handle input events.
--
-- -> (Float -> world -> IO world)	
--A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.
--
-- -> IO ()	 
--Play a game in a wi

--updateFunc :: Float -> World -> World
--updateFunc _ (x, y) = (towardCenter x, towardCenter y)
--  where
--    towardCenter :: Float -> Float
--    towardCenter c = if abs c < 0.25
--      then 0
--      else if c > 0
--        then c - 0.25
--        else c + 0.25

--drawingFunc :: World -> Picture
--drawingFunc (x, y) = undefined

--inputHandler :: Event -> World -> World
--inputHandler event (x, y) = undefined

--updateFunc :: Float -> World -> World
--updateFunc dt (x, y) = undefined


{-
inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 10)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 10)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 10, y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 10, y)
inputHandler _ w = w
-}
