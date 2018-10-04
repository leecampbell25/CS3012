import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Tree


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
  describe "Testing tree build" $ do
    it "returns " $ do
      plant [5] `shouldBe` 5
