-- Try it out:
-- runhaskell Bingo.hs items.txt

import System.Environment
import System.Random
import System.Random.Shuffle
import Data.List
import Control.Monad

main = do file <- fmap head getArgs
          items <- getLines file 
          seed  <- newStdGen
          let shuffledItems = shuffle'' seed items
          let size = 5
          print $ bingo size (take (size * size - 1) shuffledItems)

getLines = liftM lines . readFile

shuffle'' seed items = shuffle' items (length items) seed

bingo size items = take size $ chunk size $ beforeFree ++ ["FREE"] ++ afterFree where 
  (beforeFree, afterFree) = splitAt (fromIntegral $ size * size `div` 2) items

chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2 where (y1, y2) = splitAt n xs
