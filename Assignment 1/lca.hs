{--
Lee Campbell | campbel2@tcd.ie | LCA - Assignment 1

References:
-- General Tree Structure:
   1. https://rextester.com/JFW57038
   2. https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell/
   3. http://learnyouahaskell.com/zippers#focusing-on-lists
   4. http://haskellbook.com/

-- DFS fucntionality:
   1. https://www.youtube.com/watch?v=fW53ckoXUKs

-- Error calling
  1. http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#t:ErrorCall

-- Thanks also to Jack C for the advice on getting started with this in Haskell
 for the first time.

--}

module LCA where

-- Define B-Tree as a type
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show
              -- either empty or it's a node that has an element and two sub-trees.
              -- a is polymorphic type

-- Order a series of ints in the structure of a b-tree
plant :: [Int] -> Tree Int
plant (x:[]) = (Node x Leaf Leaf)
plant (x:xs) = addNodes x (plant xs)
plant [] = error "no nodes given in the list"

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
dfs x Leaf = error "desired node not in the tree"

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
