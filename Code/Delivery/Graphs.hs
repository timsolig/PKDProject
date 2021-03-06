module Graphs where

import qualified Stack as S

import System.Random

import System.IO.Unsafe ( unsafeDupablePerformIO )

{-Cell
  Represents a cell in a grid.
  Holds x- and y-coordinate.
-}
type Cell = (Float,Float)


{-Wall
  Represents a wall between cells.
  Holds two coordinates of the cells on both sides of a wall.
-}
type Wall = (Cell,Cell)


{-Maze
  Represents a Maze.
  List of walls in a maze.
-}
type Maze = [Wall]


{-pickRandom list
  Returns a randomly selected element from a given list.
    RETURNS: Picks a random element from 'list'.
    EXAMPLES:

      pickRandom [1,2] == 1 || 2

-}
pickRandom :: [a] -> a
pickRandom xs = unsafeDupablePerformIO ((xs !!) <$> randomRIO (0, length xs - 1))


{-del tuple list
  Deletes a tuple, or its' reverse, from a list.
    RETURNS: Every element in list not equal to 'tuple', or it's reverse 
    EXAMPLES:

        del (1,2) [(1,2),(3,0)] == [(3,0)]
        
        del (2,1) [(1,2),(3,0)] == [(3,0)]

-}
del :: (Eq a) => (a,a) -> [(a,a)] -> [(a,a)]
del x lst
    | x `elem` lst = Prelude.filter (/= x) lst
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
createWalls :: [Cell] -> Maze
--VARIANT: length cells
createWalls [] = []
createWalls cells@(c:cs)
    | not (null (neighbours [c] cells)) = [(c,x) | x <- neighbours [c] cells] ++ createWalls cs
    | otherwise = createWalls cs


getMaze n = mazeGenerator $ createCells n

{-iterativeDFS cells
  Generates maze based on an iterative randomized depth-first-search algorithm.
    RETURNS: Walls in the generated maze with grid 'cells'.
-}
mazeGenerator :: [Cell] -> Maze
mazeGenerator cells = mazeGeneratorAux cells walls S.empty   []
    where
        walls = createWalls cells
        mazeGeneratorAux :: [Cell] -> [Wall] -> S.Stack Cell -> [Cell] -> Maze
        --VARIANT: length cells 
        mazeGeneratorAux unvisited walls stack visited

            | Prelude.null unvisited = walls

            | Prelude.null visited  = let initCell = (0,0)
                                      in  mazeGeneratorAux (del initCell unvisited) walls (S.push initCell stack) (initCell : visited)
            | not (S.isEmpty stack) = let (currentCell, updatedStack) = S.pop stack
                                      in  case length $ neighbours [currentCell] unvisited of

                                                                                              0 -> mazeGeneratorAux unvisited walls updatedStack visited

                                                                                              _ -> let chosenNeighbour = pickRandom $ neighbours [currentCell] unvisited
                                                                                                   in  mazeGeneratorAux (del chosenNeighbour unvisited) (del (currentCell, chosenNeighbour) walls) (S.push chosenNeighbour (S.push currentCell updatedStack)) (chosenNeighbour : visited)


{-neighbours cells list
 Gives the unvisited cells that are neighbouring a specific wall
    RETURNS: unvisited in list that are adjacent to cells
-}
neighbours :: [Cell] -> [Cell] -> [Cell]
--VARIANT: lnegth list
neighbours [] _ = []
neighbours (c:cs) cells = cellNeighbours c cells ++ neighbours cs cells
    where
        cellNeighbours :: Cell -> [Cell] -> [Cell]
        cellNeighbours _ [] = []
        cellNeighbours edge@(i,j) edgeList@((i',j'):lst)
            | i' == i && abs (j-j') == 1   = (i',j') : cellNeighbours edge lst
            | j' == j && abs (i-i') == 1   = (i',j') : cellNeighbours edge lst
            | otherwise = cellNeighbours edge lst
