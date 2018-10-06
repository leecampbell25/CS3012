import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import LCA


{-
SAMPLE CODE from
http://hspec.github.io/
main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)
    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException -}

main :: IO ()
main = hspec $ do

  describe "Testing DFS function" $ do
    it "returns int[] indicating dfs path to a node " $ do
      dfs 1 (plant[1,2,10,12,9]) `shouldBe` [9,2,1]
    it "returns int[] indicating dfs path to a node " $ do
      dfs 5 (plant[5,6,2,4,8,20,12]) `shouldBe` [12,8,4,6,5]

  describe "Testing dfsMatch fucntion" $ do
    it "returns int[] of a matching path between two dfs paths" $ do
      dfsMatch (dfs 1 (plant[1,2,10,12,9])) (dfs 1 (plant[1,2,10,12,9]))`shouldBe` [9,2,1]
    it "find the common steps in the dfs paths of node 1 and node 2" $ do
      dfsMatch (dfs 1 (plant[1,2,10,12,9])) (dfs 2 (plant[1,2,10,12,9]))`shouldBe` [9,2]
