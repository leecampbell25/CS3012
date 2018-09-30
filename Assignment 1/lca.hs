data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

myTree :: Tree Int
myTree = Node 1
    (Node 2
        (Node 4 Empty Empty)
        (Node 5 Empty Empty))
    (Node 3
        (Node 6 Empty Empty)
        (Node 7 Empty Empty))
