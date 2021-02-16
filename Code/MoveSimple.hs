

field = ['x','o','o','o','o']

main :: IO ()
main = do 
  putStrLn field
  letter <- getLine
  printWithFunc letter field

--printWithFunc Char -> IO ()
printWithFunc (a:xs) b = if a == 'd' then putStr (['o','x','o','o','o']) else undefined 
    
  




--
--main :: IO ()
--main = do 
--  putStrLn "Move"
--  moveDirection <- getChar
--  moveIt (read moveDirection::Char) pos
--
--    where 
--        moveIt moveDirection pos = if moveDirection == d' then (pos +1) else undefined

