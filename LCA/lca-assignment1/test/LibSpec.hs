module LibSpec where

import           Lib
import           Test.Hspec
import           Test.QuickCheck
import Control.Exception (evaluate)

testTree1 = plant[1,2,10,12,9]
testTree2 = plant[5,6,2,4,8,20,12]
testTree3 = plant[25,29,40,10,4,90]
testTree4 = plant[1]
testTree5 = plant[8,5,1,2]


spec :: Spec
spec = do

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
