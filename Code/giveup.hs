import Graphs
import Graphics.Gloss

type PlayerPosition = (Float, Float)

windowDisplay :: Display
windowDisplay = InWindow "A Mazing Game" (200 + round windowSize, 200 + round windowSize) (10, 10)

startGame :: Float -> Float -> IO ()
startGame windowSize gridSize =
    main :: IO ()
    main = display
            windowDisplay
            white
            Line [(0,0), (1000,1000)]