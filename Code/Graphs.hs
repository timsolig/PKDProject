
import qualified Stack as S

import System.Random

import System.IO.Unsafe ( unsafeDupablePerformIO )

import Data.Set
type Cell = (Int,Int)
type Wall = (Cell,Cell)

type Maze = ([Cell],[Wall])



{-pickRandom list
Something weird with IO monads and shit. And some randomness.
    RETURNS: Picks a random element from list
    EXAMPLES:
        pickRandom [1,2] == 1 || 2
-}
pickRandom :: [a] -> a
pickRandom xs = unsafeDupablePerformIO (fmap (xs !!) $ randomRIO (0, length xs - 1))



-- createClosedMaze :: Int -> Maze
-- createClosedMaze n = foo n n 
--     where 
--         foo :: Int -> Int -> Maze
--         foo x n 
--             | x == -1 = [replicate (n+2) Cell]
--             | x == n = foo (x-1) n ++ [replicate (n+2) Cell] 
--             | otherwise  = foo (x-1) n ++ [Cell : replicate n Closed ++ [Cell]]

-- wallsToMaze :: [Cell] -> Maze
-- wallsToMaze e = x
--     where x = [sqrt $ length e ]



{-del element list
  Deletes an element from a list
    RETURNS: Every element in list not equal to element 
-}
del :: Eq a => a -> [a] -> [a]
del x lst = Prelude.filter (/= x) lst


{-createCells num
  Creates a grid of cells
    PRE: num > 0
    RETURNS: Cartesian coordinates of the (num x num)-grid
    EXAMPLES:
        createCells 3 == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)
    -}
createCells :: Int -> [Cell]
createCells n =           [(i,j) | i <- [0..n-1], j <- [0..n-1]]


createWalls :: Int -> [Wall]
createWalls n = [((i,j),(i',j')) | i <- [0..n-1], j <- [0..n-1], i' <- [0..n-1], j' <- [0..n-1], (abs (i-i') == 1  && j == j') || (abs (j-j') == 1 && i == i')]


{- prim cells
 Generates a maze basen on Prim's randomized algorithm.
    PRE: length cells > 0 
    RETURNS: Pairs from cells which are walls in the generated maze. 

-}
prim :: [Cell] -> Maze
prim = primAux [] []
    where
        {-Auxiliary function holding the set of paths and walls as well as the original cells-}
        primAux :: [Cell] -> [Wall]-> [Cell] -> Maze
        primAux pathSet wallSet unvisitedCells
            | Prelude.null pathSet = let startCell = (0,0) in primAux (startCell : pathSet) wallSet (del startCell unvisitedCells)

            | not (Prelude.null unvisitedCells) = let randomCell = pickRandom (neighbours pathSet unvisitedCells) in case length $ neighbours [randomCell] pathSet of

                                            1 -> primAux (randomCell : pathSet) (wallSet) (del randomCell unvisitedCells)
                                            _ -> primAux (randomCell : pathSet) ((randomCell, pickRandom (neighbours [randomCell] pathSet)) : wallSet) (del randomCell unvisitedCells)

            | otherwise = (pathSet,wallSet)




randomizedDFS :: [Cell] -> Maze
randomizedDFS = undefined


{-neighbours cells list
 Gives the unvisitedCells that are neighbouring a specific edge
    RETURNS: unvisitedCells in edgeList that are adjacent to edge
-}
neighbours :: [Cell] -> [Cell] -> [Cell]
neighbours [] _ = []
neighbours (c:cs) cells = cellNeigbours c cells ++ neighbours cs cells
-- where
cellNeigbours :: Cell -> [Cell] -> [Cell]
cellNeigbours _ [] = []
cellNeigbours edge@(i,j) edgeList@((i',j'):lst)
    | i' == i && ( abs (j-j') == 1 )   = (i',j') : cellNeigbours edge lst
    | j' == j && ( abs (i-i') == 1 )   = (i',j') : cellNeigbours edge lst
    | otherwise = cellNeigbours edge lst



{-
             (i-1,j)
                |
    (i,j-1)-- (i,j) -- (i,j+1)
                |
             (i+1,j)
-}

