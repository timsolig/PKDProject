import Move

import Render

import GameData

import Graphs 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Test.HUnit

{-initialState icon1 icon2 
  Takes two pictures and returns the initial state of a game.
    RETURNS: The initial state for a game where player icon i "icon1" and goal icon is "icon2".
    EXAMPLES:
-}
initialState :: Picture -> Picture -> GameState
initialState playerIcon goalIcon = Game {
        startMenu    = True,
        goalMenu     = False,
        gridSize     = undefined,
        mazePicture  = undefined,
        walls        = undefined,
        playerCoords = undefined,
        playerLevel  = 0,
        goalCoords   = undefined,
        steps        = undefined,
        playerIcon   = playerIcon,
        goalIcon     = goalIcon,
        seconds      = undefined
    }
  

{- main
  Initiates the game
  RETURNS: Gloss game
-}
main :: IO ()
main = do
    --Translates *.bmp file to gloss picture
    playerIcon <- loadBMP "5.bmp"
    goalIcon   <- loadBMP "6.bmp"
    play 
      window 
      background 
      30 --fps
      (initialState playerIcon goalIcon) 
      render 
      handleKeys 
      counter



{-Test for PlayGame.hs-}
test1 :: Test
test1 = TestCase $ assertEqual "Start menu active at beginning" (startMenu (initialState Blank Blank)) True


{-Test for Move.hs-}
test2 :: Test
test2 = TestCase $ assertEqual "A valid move" (validMove (0,0) (0,1) 10 [((0,0),(1,0))]) True

test3 :: Test
test3 = TestCase $ assertEqual "An unvalid move" (validMove (0,0) (0,1) 10 [((0,0),(0,1))]) False

test4:: Test
test4 = TestCase $ assertEqual "A valid move" (validMove (0,0) (0,1) 10 [((0,0),(1,0))]) True



{-Test for Graphs.hs-}
test5 :: Test
test5 = TestCase $ assertEqual "Deleted from list" (del (1,2) [(0,0),(1,1),(1,2)]) [(0,0),(1,1)]

test6 :: Test
test6 = TestCase $ assertEqual "Deleted reverse from list" (del (2,1) [(0,0),(1,1),(1,2)]) [(0,0),(1,1)]

test7 :: Test
test7 = TestCase $ assertEqual "Number of cells" (length $ createCells 10) (10 ^ 2)

test8 :: Test
test8 = TestCase $ assertEqual "Created cells" (createCells 2) [(0,0),(0,1),(1,0),(1,1)]


{-Test for Render.hs-}
test9 :: Test
test9 = TestCase $ assertEqual "Translated coordinates" (translateCoordinates (0, 0) 25) (-480.0,480.0)





runtests = runTestTT $ TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6,
    TestLabel "test7" test7,
    TestLabel "test8" test8,
    TestLabel "test9" test9
  ]