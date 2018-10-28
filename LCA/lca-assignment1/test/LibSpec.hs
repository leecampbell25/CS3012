module LibSpec where

import           Lib
import           Test.Hspec
import           Test.QuickCheck
import Control.Exception (evaluate)
--------------------------LCA Binary Tree---------------------------------------
testTree1 = plant[1,2,10,12,9]
testTree2 = plant[5,6,2,4,8,20,12]
testTree3 = plant[25,29,40,10,4,90]
testTree4 = plant[1]
testTree5 = plant[8,5,1,2]

----------------------------LCA DAG---------------------------------------------
---TEST DAG 1----
a = addVertex (Dag [][]) (Weight (1::Int)) -- 0
b = addVertex a (Weight 2)                 -- 1
c = addVertex b (Weight 3)                 -- 2
d = addVertex c (Weight 4)                 -- 3
e = addVertex d (Weight 5)                 -- 4
f = addVertex e (Weight 6)                 -- 5
g = addVertex f (Weight 7)                 -- 6

h = addEdge g 0 1 (Weight 8)
i = addEdge h 0 2 (Weight 9)
j = addEdge i 1 4 (Weight 10)
k = addEdge j 1 3 (Weight 11)
l = addEdge k 2 3 (Weight 12)
m = addEdge l 2 4 (Weight 13)
n = addEdge m 4 5 (Weight 14)
o = addEdge n 4 6 (Weight 15)

---TEST DAG 2---
a2 = addVertex (Dag [][]) (Weight (1::Int)) -- 0
b2 = addVertex a2 (Weight 2)                 -- 1
c2 = addVertex b2 (Weight 3)                 -- 2
d2 = addVertex c2 (Weight 4)                 -- 3
e2 = addVertex d2 (Weight 5)                 -- 4
f2 = addVertex e2 (Weight 6)                 -- 5
g2 = addVertex f2 (Weight 7)                 -- 6

h2 = addEdge g2 0 1 (Weight 8)
i2 = addEdge h2 0 2 (Weight 9)
j2 = addEdge i2 1 4 (Weight 10)
k2 = addEdge j2 1 3 (Weight 11)
l2 = addEdge k2 2 3 (Weight 12)
m2 = addEdge l2 2 4 (Weight 13)
n2 = addEdge m2 4 5 (Weight 14)
o2 = addEdge n2 4 6 (Weight 15)

----------------------------Start Tests-----------------------------------------
spec :: Spec
spec = do

  describe "Testing DAG LCA" $ do
    it "return the LCA of node 4 and node 3 in the test DAG" $ do
      dag_lca o 4 3 `shouldBe` 2
    it "return the LCA of node 5 and node 6 in the test DAG" $ do
      dag_lca o 5 6 `shouldBe` 4
    it "return the LCA of node 5 and node 6 in the test DAG" $ do
      dag_lca o 3 6 `shouldBe` 2

  describe "Test Error edge cases" $ do
    it "Returns error for adding edge that creates a cycle" $ do
      evaluate (addEdge o 6 4 (Weight 16)) `shouldThrow` errorCall "Cycle detected!"

  describe "Code coverage extras" $ do
    it "Returns error for empty path" $ do
      evaluate (pathCost o getWeightVertex getWeightEdge []) `shouldThrow` errorCall "empty path"
    it "Sepcial case where 1 element in path" $ do
      pathCost o getWeightVertex getWeightEdge [1] `shouldBe` 2
    it "Special case where 2 elements in path" $ do
      pathCost o getWeightVertex getWeightEdge [0,1] `shouldBe` 11


--------------------------LCA Binary Tree---------------------------------------
  describe "Testing plant function" $ do
    it "an empty list cannot be used to build a b-tree" $ do
      evaluate (plant[]) `shouldThrow` errorCall "Empty list"

  describe "Testing DFS function" $ do
    it "returns int[] indicating dfs path to a node " $ do
      dfs 1 testTree1 `shouldBe` [9,2,1]
    it "returns int[] indicating dfs path to a node " $ do
      dfs 5 testTree2 `shouldBe` [12,8,4,6,5]
    it "returns int[] indicating dfs path to a node " $ do
      dfs 40 testTree3 `shouldBe` [90,4,10,40]
    it "returns int[] indicating dfs path to a node " $ do
      dfs 1 testTree4 `shouldBe` [1]
  describe "Testing patternMatch fucntion" $ do
    it "returns int[] of a matching path between two dfs paths" $ do
      patternMatch (dfs 1 testTree1) (dfs 1 testTree1)`shouldBe` [9,2,1]
    it "find the common steps in the dfs paths of node 1 and node 2" $ do
      patternMatch (dfs 1 testTree1) (dfs 2 testTree1)`shouldBe` [9,2]
    it "find the common steps in the dfs paths of node 1 and node 2" $ do
      patternMatch (dfs 1 testTree1) (dfs 2 testTree5)`shouldBe` []
    it "find the common steps in the dfs paths of node 1 and node 2" $ do
      patternMatch (dfs 1 testTree5) (dfs 2 testTree1)`shouldBe` []
    it "no common path" $ do
      patternMatch (dfs 1 (plant[1])) (dfs 8 (plant[8]))`shouldBe` []
    it "Return error because can't find element in dfs" $ do
      evaluate (patternMatch (dfs 2 (plant[1])) (dfs 7 (plant[8,1,7])))`shouldThrow` errorCall "No element"

  describe "Testing LCA fucntion" $ do
    it "returns int[] of a matching path between two dfs paths" $ do
      lca 1 2 (plant[1,2,10,12,9])`shouldBe` 9
