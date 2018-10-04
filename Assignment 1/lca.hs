-- https://www.youtube.com/watch?v=fW53ckoXUKs


module Tree where

-- Define B-Tree as a type
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show
              -- either empty or it's a node that has an element and two sub-trees.

-- Plant tree
plant :: [Int] -> Tree Int
plant (x:[]) = (Node x Leaf Leaf)
plant (x:xs) = grow x (plant xs)

--Grow tree
grow :: Int -> Tree Int -> Tree Int
grow x Leaf = (Node x Leaf Leaf)
grow x (Node i leftTree rightTree)
      | x <= i (Node i (grow x leftTree) rightTree)
      | otherwise = (Node i leftTree (grow x rightTree))


-- TODO LCA Functions

-- DFS fucntion

dfs :: Int -> Tree Int -> [Int]
dfs x (Node i leftTree rightTree)
      | x == i = i:[]
      | x < i = i:(dfs x leftTree)
      | otherwise = i:(dfs x rightTree)

-- find last common entry in both DFS arrays
