
module Graphs (Maze,Cell,Wall,createCells,createWalls,prim,recurDFS,iterDFS) where

import qualified Stack as S

import System.Random

import System.IO.Unsafe ( unsafeDupablePerformIO )


type Cell = (Float,Float)

type Wall = (Cell,Cell)

type Maze = [Wall]

{-pickRandom list
Something weird with IO monads and shit. And some randomness.
    RETURNS: Picks a random element from list
    EXAMPLES:
        pickRandom [1,2] == 1 || 2
-}
pickRandom :: [a] -> a
pickRandom xs = unsafeDupablePerformIO (fmap (xs !!) $ randomRIO (0, length xs - 1))
-- pickRandom xs = unsafeDupablePerformIO (xs !!) <$> randomRIO (0, length xs - 1)



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
    RETURNS: Cartesian coordinates of the cells in the (num x num)-grid
    EXAMPLES:

        createCells 0 == []

        createCells 2 == [(0,0),(0,1),(1,0),(1,1)]
    
    -}

createCells :: Float -> [Cell]
createCells n = [(i,j) | i <- [0..n-1], j <- [0..n-1]]



{-createWalls cells
  Creates walls between all cells in a 
    RETURNS: Walls between all neighbouring cells in cells
    EXAMPLES: 
    
        createWalls $ createCells 0 == []
        
        createWalls $ createCells 2 == [((0,0),(0,1)),((0,0),(1,0)),((0,1),(1,1)),((1,0),(1,1))]

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
prim cells = primAux [] [] cells
    where
        {-Auxiliary function holding the set of paths and walls as well as the original cells-}
        primAux :: [Cell] -> [Wall]-> [Cell] -> Maze
        primAux visited walls unvisited
            
            | Prelude.null visited = let 
                                         initCell = pickRandom unvisited 
                                     in 
                                         primAux (initCell : visited) walls (del initCell unvisited)

            | not (Prelude.null unvisited) = let 
                                                randomCell = pickRandom (neighbours visited unvisited) 
                                             in 
                                                case length $ neighbours [randomCell] visited of
            
            
                    1 -> primAux (randomCell : visited) ((randomCell, pickRandom (neighbours [randomCell] visited)) : walls) (del randomCell unvisited)
                   
                    _ -> primAux (randomCell : visited) walls (del randomCell unvisited)
            

            | otherwise = walls







{-iterativeDFS cells
  Generates maze based on an iterative randomized depth-first-search algorithm.
    RETURNS:

-}
iterDFS :: [Cell] -> Maze
iterDFS cells = iterDFSaux cells walls S.empty   [] 
    where
        walls = createWalls cells
        iterDFSaux :: [Cell] -> [Wall] -> S.Stack Cell -> [Cell] -> Maze
        
        iterDFSaux unvisited walls stack visited 
            
            | Prelude.null unvisited = walls

            | Prelude.null visited = let initCell = pickRandom unvisited in iterDFSaux (del initCell unvisited) walls (S.push initCell stack) (initCell : visited)
            
            | not (S.isEmpty stack) = let currentCell = S.top stack in case length $ neighbours [currentCell] unvisited of
                    
                0 -> iterDFSaux unvisited walls stack visited
                    
                _ -> let unvisitedNeighbour = pickRandom $ neighbours [currentCell] unvisited in iterDFSaux (del unvisitedNeighbour unvisited) (del (currentCell,unvisitedNeighbour) walls) (S.push unvisitedNeighbour $ S.push currentCell stack) (unvisitedNeighbour : visited)



{-recurDFS cells
  Generates a maze based on the recursie Depth-First-Algorithm
-}
recurDFS :: [Cell] -> Maze
recurDFS cells = let initCell = pickRandom cells 
                 in 
                     recurDFSaux cells walls S.empty [] initCell
    where 
        walls = createWalls cells

        recurDFSaux :: [Cell] -> [Wall] -> S.Stack Cell -> [Cell] -> Cell -> Maze
        recurDFSaux unvisited walls stack visited currentCell
           
            | length visited < length unvisited = let unvisitedNeighbours = neighbours [currentCell] unvisited in case length unvisitedNeighbours of    
                                                                                                
                0 -> recurDFSaux unvisited walls updatedStack visited newCell 
                        where (newCell, updatedStack) = S.pop stack 
                
                _ -> recurDFSaux unvisited (del (currentCell, newCell) walls) (S.push currentCell stack) (currentCell : visited) newCell
                        where newCell = pickRandom unvisitedNeighbours

            | otherwise = walls



-- divisionAlg :: [Cell] -> Maze
-- divisionAlg cells = divisionAlgAux cells []

{-neighbours cells list
 Gives the unvisited that are neighbouring a specific edge
    RETURNS: unvisited in edgeList that are adjacent to edge
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
