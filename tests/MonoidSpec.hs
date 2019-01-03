module MonoidSpec where

import           Control.Exception (evaluate)
import           Monoid
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = describe "Monoids" $ do
    describe "List" $ do
        it "combines" $ do
          bothLists `shouldBe` [1,2,3,4,5,6]
        it "empty item" $ do
          addNothing `shouldBe` [1,2,3]

    it "calculates 10 with MySum" $
        ten `shouldBe` 10
    it "calculates 66 with MyProduct" $
        sixtySix `shouldBe` 66
