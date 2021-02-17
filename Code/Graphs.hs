
import qualified Stack as S 

import System.Random

-- data Cell = Cell 
--           | Closed
--           | Open deriving (Eq, Show)

import System.Random
 

type Maze = [[Cell]]

type Cell = (Int,Int)


data RandomNumber = Num Int

-- foo :: IO Int -> RandomNumber'
-- foo n = Num n

pickRandom :: [a] -> IO a
pickRandom xs = fmap (xs !!) $ randomRIO (0, length xs - 1)



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


-- showMaze:: Maze -> IO ()

-- showMaze [[x]] = do print x
            
-- showMaze (x:xs) =  do
--                     print (x) 
--                     then
--                         show (showMaze xs)


del :: Eq a => a -> [a] -> [a]
del x = filter (/= x)


createWalls :: Int -> [Cell]
createWalls n = [(i,j) | i <- [0..n-1], j <- [0..n-1] ]



prim :: [Cell] -> [Cell]
prim cells = primAux [] [] cells 

primAux :: [Cell] -> [Cell]-> [Cell] -> [Cell]
primAux pathSet wallSet unvisitedCells
    | null pathSet = let startCell = (0,0) in primAux [startCell] (neighbours startCell unvisitedCells) (del startCell unvisitedCells) 

    | not (null unvisitedCells) = let randomCell = (pickRandom wallSet) in case length $ neighbours randomCell pathSet of 
                                    1 -> primAux (randomCell : pathSet) (neighbours randomCell unvisitedCells ++ wallSet) (del randomCell unvisitedCells)
                                    _ -> primAux pathSet wallSet unvisitedCells
    | otherwise = pathSet





{-neighbours edge edgeList
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
neighbours :: Cell -> [Cell] -> [Cell]
neighbours _ [] = []
neighbours edge@(i,j) edgeList@((i',j'):lst)
    | i' == i && ( abs (j-j') == 1 )   = (i',j') : neighbours edge lst
    | j' == j && ( abs (i-i') == 1 )   = (i',j') : neighbours edge lst
    | otherwise = neighbours edge lst
