module Tree where

-- Define B-Tree as a type
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show
              -- either empty or it's a node that has an element and two sub-trees.

-- Plant tree
plant :: [Int] -> Tree Int
plant (x:[]) = (Node x Empty Empty)
plant (x:xs) = branch x (plant xs)

--Grow tree
branch :: Int -> Tree Int -> Tree Int
branch x Empty = (Node x Empty Empty)
branch x (Node y leftBranch rightBranch)
      | x <= i Node i ()
