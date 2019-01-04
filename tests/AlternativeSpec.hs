module AlternativeSpec where

import           Alternative
import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Alternative" $ do
    it "returns nope" $
      nah `shouldBe` Nope
    it "returns the first item" $
      fallback `shouldBe` Yeah 1
    it "returns the second item" $
      found `shouldBe` Yeah 2
