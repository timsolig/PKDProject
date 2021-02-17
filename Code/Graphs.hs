
module Graphs (Maze, generateMaze) where

import qualified Stack as S

import System.Random

import System.IO.Unsafe ( unsafeDupablePerformIO )


import Data.Set
type Cell = (Int,Int)
type Wall = (Cell,Cell)

type Maze = ([Cell],[Wall])



generateMaze :: Int -> Maze
generateMaze seed =let cells = createCells seed in case seed of 
                                1 -> prim cells
                                2 -> recurDFS cells
                                3 -> iterDFS cells

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
del :: (Eq a) => (a,a) -> [(a,a)] -> [(a,a)]
del x lst 
    | elem x lst = Prelude.filter (/= x) lst
    | otherwise  = del (rev x) lst

rev :: (a,a) -> (a,a) 
rev (x,y) = (y,x)

{-createCells num
  Creates the cell coordinates of a grid
    PRE: num > 0
    RETURNS: Cartesian coordinates of the (num x num)-grid
    EXAMPLES:
        createCells 3 == [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)
    -}

createCells :: Int -> [Cell]
createCells n = [(i,j) | i <- [0..n-1], j <- [0..n-1]]



{-createWalls cells
  Creates walls between all cells in a 
-}
createWalls :: [Cell] -> [Wall]
createWalls [] = []
createWalls cells@(c:cs)
    | (length $ neighbours [c] cells) > 0 = [(c,x) | x <- neighbours [c] cells] ++ createWalls cs
    | otherwise = createWalls cs





{- prim cells
 Generates a maze basen on Prim's randomized algorithm.
    RETURNS: Pairs from cells which are walls in the generated maze. 

-}
prim :: [Cell] -> Maze
prim = primAux [] []
    where
        {-Auxiliary function holding the set of paths and walls as well as the original cells-}
        primAux :: [Cell] -> [Wall]-> [Cell] -> Maze
        primAux visited wallSet unvisited
            
            | Prelude.null visited = let startCell = pickRandom unvisited in primAux (startCell : visited) wallSet (del startCell unvisited)

            | not (Prelude.null unvisited) = let randomCell = pickRandom (neighbours visited unvisited) in case length $ neighbours [randomCell] visited of
            
                    1 -> primAux (randomCell : visited) (wallSet) (del randomCell unvisited)
            
                    _ -> primAux (randomCell : visited) ((randomCell, pickRandom (neighbours [randomCell] visited)) : wallSet) (del randomCell unvisited)

            | otherwise = (visited,wallSet)




{-iterativeDFS cells
  Generates maze based on an iterative randomized depth-first-search algorithm.
    
-}
iterDFS :: [Cell] -> Maze
iterDFS cells = iterDFSaux cells walls S.empty   [] 
    where
        walls = createWalls cells
        iterDFSaux :: [Cell] -> [Wall] -> S.Stack Cell -> [Cell] -> Maze
        
        iterDFSaux unvisited walls stack visited 
            
            | Prelude.null visited = let startCell = pickRandom unvisited in iterDFSaux (del startCell unvisited) walls (S.push startCell stack) (startCell : visited)
            
            | Prelude.null unvisited = (visited,walls)
            
            | not (S.isEmpty stack) = let currentCell = S.top stack in case length $ neighbours [currentCell] unvisited of
                    
                0 -> iterDFSaux unvisited walls stack visited
                    
                _ -> let chosenCell = pickRandom $ neighbours [currentCell] unvisited in iterDFSaux (del chosenCell unvisited) (del (currentCell,chosenCell) walls) (S.push chosenCell $ S.push currentCell stack) (chosenCell : visited)

{-recurDFS cells
  Generates a maze based on the recursie Depth-First-Algorithm
-}
recurDFS :: [Cell] -> Maze
recurDFS cells = let startCell = pickRandom cells in recurDFSaux cells walls S.empty [] startCell
    where 
        walls = createWalls cells

        recurDFSaux :: [Cell] -> [Wall] -> S.Stack Cell -> [Cell] -> Cell -> Maze
        recurDFSaux unvisited walls stack visited currentCell
           
            | length visited < length unvisited = let unvisitedNeighbours = neighbours [currentCell] unvisited in case length unvisitedNeighbours of    
                                                                                                
                0 -> recurDFSaux unvisited walls updatedStack visited newCell 
                        where (newCell, updatedStack) = S.pop stack 
                
                _ -> recurDFSaux unvisited (del (currentCell, newCell) walls) (S.push currentCell stack) (currentCell : visited) newCell
                        where newCell = pickRandom unvisitedNeighbours

            | otherwise = ([],walls)





{-neighbours cells list
 Gives the unvisited that are neighbouring a specific edge
    RETURNS: unvisited in edgeList that are adjacent to edge
-}
neighbours :: [Cell] -> [Cell] -> [Cell]
neighbours [] _ = []
neighbours (c:cs) cells = cellNeigbours c cells ++ neighbours cs cells
    where
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

