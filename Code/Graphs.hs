

data Cell = Edge | Closed | Path deriving (Show)

type Maze = [[Cell]]


createClosedMaze :: Int -> Maze
createClosedMaze n = foo n n 
    where 
        foo :: Int -> Int -> Maze
        foo x n 
            | x == -1 = [replicate (n+2) Edge]
            | x == n = foo (x-1) n ++ [replicate (n+2) Edge] 
            | otherwise  = foo (x-1) n ++ [Edge : replicate n Closed ++ [Edge]]


showMaze:: Maze -> String
showMaze [[x]] = show x
showMaze (x:xs) =  show x ++ "\n" ++ showMaze xs



prim :: Maze -> Maze
prim m = primAux m []


