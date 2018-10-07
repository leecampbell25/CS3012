-- https://www.youtube.com/watch?v=fW53ckoXUKs

module LCA where

-- Define B-Tree as a type
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show
              -- either empty or it's a node that has an element and two sub-trees.

-- Plant tree
plant :: [Int] -> Tree Int
plant (x:[]) = (Node x Leaf Leaf)
plant (x:xs) = addNodes x (plant xs)
plant [] = error "no nodes given in the list"

-- Add children to tree
addNodes:: Int -> Tree Int -> Tree Int
addNodes x Leaf = (Node x Leaf Leaf)
addNodes x (Node i leftTree rightTree)
            | x <= i = (Node i (addNodes x leftTree) rightTree)
            | otherwise = (Node i leftTree (addNodes x rightTree))

-- TODO LCA Functions

lca :: Int -> Int -> Tree Int -> Int
lca x y tree = (dfsMatch(dfs x tree)(dfs y tree))

arrayToInt:: [Int] -> Int
arrayToInt x = head x

-- DFS fucntion
dfs :: Int -> Tree Int -> [Int]
dfs x (Node i leftTree rightTree)
      | x == i = i:[]
      | x < i = i:(dfs x leftTree)
      | otherwise = i:(dfs x rightTree)
dfs x Leaf = error "desired node not in the tree"

-- find last common entry in both DFS arrays

dfsMatch :: [Int] -> [Int] -> [Int]
dfsMatch (x:[]) (y:[])
          | x == y = x:[]
          | otherwise = []

dfsMatch (x:xs) (y:ys)
          | x == y = x:(dfsMatch xs ys)
          | otherwise = []

dfsMatch (x:xs) [] = []
dfsMatch [] (y:ys) = []
