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
