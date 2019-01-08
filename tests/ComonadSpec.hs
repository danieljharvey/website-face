module ComonadSpec where

import           Comonad
import           Control.Comonad.Store
import           Control.Exception     (evaluate)
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Which Comonad" $ do
    it "currentPosition" $
        currentPosition `shouldBe` This
    it "somethingElse" $
        somethingElse `shouldBe` "that other thing"
    it "swapped" $
        swapped `shouldBe` "that other thing"
    it "itsThat" $
        itsThat `shouldBe` That
    it "nowItsWho" $
        nowItsWho `shouldBe` Who
    it "what" $
        what `shouldBe` Just "this thing"
    it "otherWhat" $
        otherWhat `shouldBe` Just "that other thing"
    it "otherOtherWhat" $
        otherOtherWhat `shouldBe` Nothing
  describe "Battenburg" $ do
    it "seems ok" $
        getBattenType (First, First) `shouldBe` Pink
    it "seems ok" $
        getBattenType (Middle, First) `shouldBe` Other
  describe "Grid" $ do
    it "Gets 0,0" $
      getGridItem startGrid (0,0) `shouldBe` 0
    it "Gets 2,2" $
      getGridItem startGrid (2,2) `shouldBe` 1
    it "Gets 5,5" $
      getGridItem startGrid (5,5) `shouldBe` 0
