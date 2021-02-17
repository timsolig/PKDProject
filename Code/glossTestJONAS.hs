
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Graphics.Gloss.Interface.Pure.Game

--import Graphics.Gloss.Data
--import Graphics.Gloss.Data.SpecialKey

type World = (Float,Float)

main :: IO ()
main = play
  windowDisplay -- size of window
  white --color
  20 --fps
  (0,0) --Initial World
  drawPlayfield
  inputHandler
  updateFunc

hej ::Float -> Float -> Picture
hej x y= Pictures [color black (line [ (-100, -300), (-100,  300) ]) <>
                color black (line [ ( 100, -300), ( 100,  300) ]) <>
                color black (line [ (-300,  100), ( 300,  100) ]) <>
                color black (line [ (-300, -100), ( 300, -100) ]) <>
                translate x y (color red (circleSolid 100))]


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
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 200)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 200)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 200, y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 200, y)
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
