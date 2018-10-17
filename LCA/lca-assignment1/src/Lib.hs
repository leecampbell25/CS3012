module Lib
    ( plant,
      addNodes,
      lca,
      dfs,
      dfsMatch,
      Tree(..),
    ) where




-- Define B-Tree as a type
data Tree a = Leaf | Node a (Tree a) (Tree a)
              -- either empty or it's a node that has an element and two sub-trees.
              -- a is polymorphic type

-- Order a series of ints in the structure of a b-tree
plant :: [Int] -> Tree Int
plant (x:[]) = (Node x Leaf Leaf)
plant (x:xs) = addNodes x (plant xs)
plant [] = error "Empty list"

-- Add nodes to the tree
addNodes:: Int -> Tree Int -> Tree Int
addNodes x Leaf = (Node x Leaf Leaf)
addNodes x (Node i leftTree rightTree)
            | x <= i = (Node i (addNodes x leftTree) rightTree)
            | otherwise = (Node i leftTree (addNodes x rightTree))

-- Lowest Common Ancestor Functions
lca :: Int -> Int -> Tree Int -> Int
lca x y tree = head(dfsMatch(dfs x tree)(dfs y tree)) -- head extracts the answer from a [] to allow an int to be returned

-- Depth First Search fucntion
dfs :: Int -> Tree Int -> [Int]
dfs x (Node i leftTree rightTree)
      | x == i = i:[]
      | x < i = i:(dfs x leftTree)
      | otherwise = i:(dfs x rightTree)
dfs x Leaf = error "No element"


-- Find matching DFS paths in full or part
dfsMatch :: [Int] -> [Int] -> [Int]
dfsMatch (x:[]) (y:[])
          | x == y = x:[]
          | otherwise = []

dfsMatch (x:xs) (y:ys)
          | x == y = x:(dfsMatch xs ys)
          | otherwise = []

dfsMatch (x:xs) [] = []
dfsMatch [] (y:ys) = []
