
import qualified Stack as S 

import System.Random 

import System.IO.Unsafe ( unsafeDupablePerformIO )

-- data Cell = Cell 
--           | Closed
--           | Open deriving (Eq, Show)

 
type Cell = (Int,Int)
type Wall = (Cell,Cell)

type Maze = ([Cell],[Wall])




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




del :: Eq a => a -> [a] -> [a]
del x lst = filter (/= x) lst



{-createCells num
Creates a grid of cells
    PRE: num > 0
    RETURNS: Cartesian coordinates of the (num x num)-grid
    -}
createCells :: Int -> [Cell]
createCells n = [(i,j) | i <- [0..n-1], j <- [0..n-1] ]

{- prim cells
 Generates a maze basen on the Prim's randomized algorithm
    PRE: length cells > 0 
    RETURNS: 

-}
prim :: [Cell] -> Maze
prim cells = primAux [] [] cells 
    where
        {-Auxiliary function holding the set of paths and walls as well as the original cells-}
        primAux :: [Cell] -> [Wall]-> [Cell] -> Maze
        primAux pathSet wallSet unvisitedCells
            | null pathSet = let startCell = (0,0) in primAux (startCell : pathSet) wallSet (del startCell unvisitedCells) 
            
            | not (null unvisitedCells) = let randomCell = pickRandom (neighbours pathSet unvisitedCells) in case length $ neighbours [randomCell] pathSet of 
                                            
                                            1 -> primAux (randomCell : pathSet) (wallSet) (del randomCell unvisitedCells)
                                            _ -> primAux (randomCell : pathSet) ((randomCell, pickRandom (neighbours [randomCell] pathSet)) : wallSet) (del randomCell unvisitedCells)

            | otherwise = ([],wallSet)




{-neighbours cells list
 Gives the unvisitedCells that are neighbouring a specific edge
    RETURNS: unvisitedCells in edgeList that are adjacent to edge
-}

{-
             (i-1,j)
                |
    (i,j-1)-- (i,j) -- (i,j+1)
                |
             (i+1,j)
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
