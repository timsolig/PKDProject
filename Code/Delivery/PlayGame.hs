import Move

import Render

import GameData

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
        playerLevel  = undefined,
        goalCoords   = undefined,
        steps        = undefined,
        testImageP   = playerIcon,
        testImageG   = goalIcon,
        seconds      = undefined
    }
  

{- main
  Initiates the game
  RETURNS: Gloss game
-}
main :: IO ()
main = do
    playerIcon <- loadBMP "2.bmp"
    goalIcon   <- loadBMP "2.bmp"
    
    play 
      window 
      background 
      30 --fps
      (initialState playerIcon goalIcon) 
      render 
      handleKeys 
      counter



-- test1 :: Test
test1 = TestCase $ assertEqual "Silly" (length "hej" == 3) True

-- test2 :: Test
test2 = TestCase $ assertEqual "Also silly" True True

-- test3 :: Test
test3 = TestCase $ assertEqual "Also silly" True True

-- test4 :: Test
test4 = TestCase $ assertEqual "Also silly" True True

-- test5 :: Test
test5 = TestCase $ assertEqual "Also silly" True True

-- test6 :: Test
test6 = TestCase $ assertEqual "Wow javla" True True

-- runtests = runTestTT [test1,test2,test3,test4,test5,test6]

-- runtests = runTestTT $ TestList [
--     TestLabel "test1" test1, 
--     TestLabel "test2" test2,
--     TestLabel "test3" test3
--   ]