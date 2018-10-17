module LibSpec where

import           Lib
import           Test.Hspec
import           Test.QuickCheck
import Control.Exception (evaluate)


spec :: Spec
spec = do

  describe "Testing plant function" $ do
    it "4 is not a node in the tree so should throw an error" $ do
      evaluate (plant[]) `shouldThrow` errorCall "Empty list"

  describe "Testing DFS function" $ do
    it "returns int[] indicating dfs path to a node " $ do
      dfs 1 (plant[1,2,10,12,9]) `shouldBe` [9,2,1]
    it "returns int[] indicating dfs path to a node " $ do
      dfs 5 (plant[5,6,2,4,8,20,12]) `shouldBe` [12,8,4,6,5]
   -- it "4 is not an element should return error" $ do
    --  evaluate (dfs 4 (plant[1,2,3])) `shouldThrow` errorCall "No element" -- TODO

  describe "Testing dfsMatch fucntion" $ do
    it "returns int[] of a matching path between two dfs paths" $ do
      dfsMatch (dfs 1 (plant[1,2,10,12,9])) (dfs 1 (plant[1,2,10,12,9]))`shouldBe` [9,2,1]
    it "find the common steps in the dfs paths of node 1 and node 2" $ do
      dfsMatch (dfs 1 (plant[1,2,10,12,9])) (dfs 2 (plant[1,2,10,12,9]))`shouldBe` [9,2]

  describe "Testing LCA fucntion" $ do
    it "returns int[] of a matching path between two dfs paths" $ do
      lca 1 2 (plant[1,2,10,12,9])`shouldBe` 9
