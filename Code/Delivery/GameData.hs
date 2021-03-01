module GameData where

import Graphics.Gloss
import Graphs
{-GameState 
  Represents the 

  INVARIANT: gridSize > 0
-}
data GameState = Game {
        startMenu    :: Bool,
        goalMenu     :: Bool,
        gridSize     :: Float,
        mazePicture  :: Picture,
        walls        :: Maze,
        playerCoords :: (Float, Float),
        playerLevel  :: Int,
        goalCoords   :: (Float, Float),
        steps        :: Int,
        playerIcon   :: Picture,
        goalIcon     :: Picture,
        seconds      :: Float
    } deriving (Eq,Show)

