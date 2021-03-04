module GameData where

import Graphics.Gloss
import Graphs
{-GameState 
    Represents the game session with all necessary data that varies with each level. 
    INVARIANT: gridSize > 0,
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
    } deriving (Eq, Show)

