import Control.Concurrent
 
import Graphs as A_mazeing_Stuff  
-- import Glossstuff

main :: IO ()
main = do
    putStrLn "Alright, time to work!"
    loadMsg "...doing some work..." 3
    putStrLn "You deserve a break!"
    loadMsg "...having a break..." 300
    main

loadMsg :: String -> Int -> IO ()
loadMsg _ 0 = return ()
loadMsg str x = do
    threadDelay 1000000
    putStrLn str
    loadMsg str (x-1)
