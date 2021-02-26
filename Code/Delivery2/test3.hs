import Data.Time
import System.IO.Unsafe
getTime :: UTCTime
getTime =  unsafeDupablePerformIO getCurrentTime

a = getTime

calcTime a b = diffUTCTime a b

to x = toRational x 
