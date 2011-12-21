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
          let shuffledItems = shuffle' items (length items) seed
          print $ bingo 3 shuffledItems

getLines = liftM lines . readFile

bingo size = take size . chunk size . take (size * size)

chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2 where (y1, y2) = splitAt n xs
