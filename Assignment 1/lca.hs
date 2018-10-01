data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

myTree :: Tree Int
myTree =
  Node 1
    (Node 2
        (Node 4 Empty Empty)
        (Node 5 Empty Empty))
    (Node 3
        (Node 6 Empty Empty)
        (Node 7 Empty Empty))

        lca :: Eq a => Tree a -> a -> a -> Either Bool a
        lca Empty _ _ = Left False
        lca (Node v tl tr) n1 n2 =
            let l = lca tl n1 n2
                r = lca tr n1 n2
                onroot = v == n1 || v == n2
            in case (l, r, onroot) of
                (Right a  , _         , _    ) -> Right a
                (_        , Right a   , _    ) -> Right a
                (Left True, Left True , _    ) -> Right v
                (Left True, _         , True ) -> Right v
                (_        , Left True , True ) -> Right v
                (Left True, _         , False) -> Left True
                (_         , Left True, False) -> Left True
                (_         , _        , True ) -> Left True
                _ -> Left False

        lca_show :: (Eq a, Show a, PrintfArg a) => Tree a -> a -> a -> String
        lca_show t n1 n2 = printf "LCA(%d,%d)=%s" n1 n2 result
            where result = case lca t n1 n2 of
                                Right a -> show a
                                _ -> "not found"
