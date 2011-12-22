-- Try it out:
-- runhaskell Bingo.hs items.txt

import System.Environment
import System.Random
import System.Random.Shuffle
import Data.Function
import Data.List
import Control.Monad
import Text.PrettyPrint.Leijen

main = do file <- fmap head getArgs
          items <- getLines file 
          seed  <- newStdGen
          let shuffledItems = shuffle'' seed items
          let size = 5
          putDoc $ pp $ bingo size (take (size * size - 1) shuffledItems)

getLines = liftM lines . readFile

shuffle'' seed items = shuffle' items (length items) seed

bingo size items = take size $ chunk size $ beforeFree ++ ["FREE"] ++ afterFree where 
  (beforeFree, afterFree) = splitAt (fromIntegral $ size * size `div` 2) items

chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2 where (y1, y2) = splitAt n xs

pp bingo = vcat (map format bingo) <> line where
  format row = foldl1 (<|>) $ map (fill (longestItem bingo) . text) row where
    x <|> y = x <> text " | " <> y
    longestItem bingo = length $ maximumBy (compare `on` length) $ concat bingo