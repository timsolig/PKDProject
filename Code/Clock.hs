module Clock () where

import Control.Concurrent

-- https://hackage.haskell.org/p(ackage/timers-0.2.0.4/docs/Control-Concurrent-Timer.html

countDown :: Int -> IO ()
countDown n = do
    loadMsg n 
    threadDelay 1000
    countDown (n-1)

loadMsg :: Int -> Int -> IO ()
loadMsg _ 0 = return ()
loadMsg str x = do
    threadDelay 1000000
    putStrLn str
    loadMsg str (x-1)