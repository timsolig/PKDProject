module Graphs where

import qualified Stack as S

import System.Random

import System.IO.Unsafe ( unsafeDupablePerformIO )


type Cell = (Float,Float)

type Wall = (Cell,Cell)

type Maze = [Wall]

foo ::Float -> [Wall]
foo n = mazeGenerator $ createCells n
{-pickRandom list
Something weird with IO monads and shit. And some randomness.
    RETURNS: Picks a random element from list
    EXAMPLES:
        pickRandom [1,2] == 1 || 2
-}
pickRandom :: [a] -> a
pickRandom xs = unsafeDupablePerformIO (fmap (xs !!) $ randomRIO (0, length xs - 1))


{-del tuple list
  Deletes a tuple, or its' reverse, from a list.
    RETURNS: Every element in list not equal to 'tuple', or it's reverse 
    EXAMPLES:

        del (1,2) [(1,2),(3,0)] == [(3,0)]
        
        del (2,1) [(1,2),(3,0)] == [(3,0)]

-}
del :: (Eq a) => (a,a) -> [(a,a)] -> [(a,a)]
del x lst 
    | elem x lst = Prelude.filter (/= x) lst
    | otherwise  = del (rev x) lst
        where
            rev :: (a,a) -> (a,a) 
            rev (x,y) = (y,x)



{-createCells num
  Creates the cell coordinates of a grid.
    PRE: num => 0
    RETURNS: Cartesian coordinates of the cells in the (num x num)-grid.
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
    | not (null (neighbours [c] cells)) = [(c,x) | x <- neighbours [c] cells] ++ createWalls cs
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
            
            
                    1 -> primAux (randomCell : visited) ((randomCell, pickRandom (neighbours [randomCell] unvisited)) : walls) (del randomCell unvisited)
                   
                    _ -> primAux (randomCell : visited) walls (del randomCell unvisited)
            

            | otherwise = walls




{-mazeGeneratorDFS cells
  Generates maze based on an iterative randomized depth-first-search algorithm.
    RETURNS: Walls in the generated maze based on 'cells'

-}
mazeGenerator :: [Cell] -> Maze
mazeGenerator cells = mazeGeneratoraux cells walls S.empty   [] 
    where
        walls = createWalls cells
        mazeGeneratoraux :: [Cell] -> [Wall] -> S.Stack Cell -> [Cell] -> Maze
        
        mazeGeneratoraux unvisited walls stack visited 
            
            | Prelude.null unvisited = walls

            | Prelude.null visited  = let initCell = pickRandom unvisited
                                      in  mazeGeneratoraux (del initCell unvisited) walls (S.push initCell stack) (initCell : visited)
            
            | not (S.isEmpty stack) = let (currentCell, updatedStack) = S.pop stack
                                      in  case length $ neighbours [currentCell] unvisited of
                    
                                        0 -> mazeGeneratoraux unvisited walls updatedStack visited
                                            
                                        _ -> let chosenNeighbour = pickRandom $ neighbours [currentCell] unvisited 
                                             in  mazeGeneratoraux (del currentCell (del chosenNeighbour unvisited)) (del (currentCell, chosenNeighbour) walls) (S.push chosenNeighbour (S.push currentCell updatedStack)) (chosenNeighbour : visited)




{-divisionAlgorithm cells-}


{-neighbours cells list
 Gives the unvisited that are neighbouring a specific edge
    RETURNS: unvisited in edgeList that are adjacent to edge
-}
neighbours :: [Cell] -> [Cell] -> [Cell]
neighbours [] _ = []
neighbours (c:cs) cells = cellNeighbours c cells ++ neighbours cs cells
-- where
cellNeighbours :: Cell -> [Cell] -> [Cell]
cellNeighbours _ [] = []
cellNeighbours edge@(i,j) edgeList@((i',j'):lst)
    | i' == i && ( abs (j-j') == 1 )   = (i',j') : cellNeighbours edge lst
    | j' == j && ( abs (i-i') == 1 )   = (i',j') : cellNeighbours edge lst
    | otherwise = cellNeighbours edge lst
