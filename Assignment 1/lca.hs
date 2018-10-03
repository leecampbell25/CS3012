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
grow x (Node i leftBranch rightBranch)
      | x <= i (Node i (grow x leftBranch) rightBranch)
      | otherwise = (Node i leftBranch (grow x rightBranch))


-- TODO LCA Functions 
