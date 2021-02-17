import Graphics.Gloss

main :: IO ()
main = play
  windowDisplay
  black
  20
  (0, 0)
  drawingFunc
  inputHandler
  updateFunc

windowDisplay :: Display
windowDisplay = InWindow "Testar Gloss" (1080, 1080) (10, 10)

--playIOSource#
--
-- :: forall world. Display	
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