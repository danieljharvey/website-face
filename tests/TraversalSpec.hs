module TraversalSpec where

import           Traversal
import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "traversal" $ do
    it "foldMaps tree" $
      sampleTreeTotal `shouldBe` 12
    it "traverses maybeTree" $
      justTree `shouldBe` Just (Branch (Branch (Leaf 2) (Leaf 3)) (Branch (Leaf 5) (Leaf 2)))
    it "traverses anotherMaybeTree" $
      nothingTree `shouldBe` Nothing
