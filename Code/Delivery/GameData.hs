module GameData where

import Graphics.Gloss
import Graphs
{-GameState 
  Represents the game session with all necessary data that varies with each level. 
-}
data GameState = Game {
        startMenu    :: Bool,
        goalMenu     :: Bool,
        --INVARIANT: gridSize > 0,
        gridSize     :: Float,
        mazePicture  :: Picture,
        walls        :: Maze,
        --INVARIANT:
        playerCoords :: (Float, Float),
        playerLevel  :: Int,
        --INVARIANT:
        goalCoords   :: (Float, Float),
        steps        :: Int,
        playerIcon   :: Picture,
        goalIcon     :: Picture,
        seconds      :: Float
    } deriving (Eq, Show)

