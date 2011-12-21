import Control.Monad

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

jakub = Node (Node (Leaf "Pentti")
                   "Jorma" 
                   (Leaf "Maija")) 
             "Jakub" 
             (Node (Leaf "Jozef") 
                   "Teresa" 
                   (Node Empty
                         "Jozefa" 
                         (Leaf "Salomea"))) 

father person = case person of 
  (Node Empty _ _) -> Nothing
  (Node father _ _) -> Just father
  otherwise -> Nothing

mother person = case person of 
  (Node _ _ Empty) -> Nothing
  (Node _ _ mother) -> Just mother
  otherwise -> Nothing

ancestor :: Monad m => [a -> m a] -> a -> m a
ancestor = flip (foldM (flip ($)))

name person = case person of 
  Just (Node _ name _) -> Just name
  Just (Leaf name) -> Just name
  Nothing -> Nothing
 
-- Try it out:
-- > name $ ancestor [mother, mother] jakub

-- Possible improvements
-- http://hpaste.org/55545
-- http://hpaste.org/55546
