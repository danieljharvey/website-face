module MonoidSpec where

import           Control.Exception (evaluate)
import           Monoid
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
    describe "Monoids" $ do
        it "strConcat" $ do
            strConcat "Spruce" "Goose" `shouldBe` "SpruceGoose"
        it "myFoldr" $ do
            myFoldr strConcat "" ["hot","dog","frog"] `shouldBe` "hotdogfrog"
        it "output1" $ do
            output1 `shouldBe` "hotdogfrog"
        it "output2" $ do
            output2 `shouldBe` ""
        it "output3" $ do
            output3 `shouldBe` "sorry, no value"
        it "output4" $ do
            output4 `shouldBe` "hotdogfrogsorry, no value"
        it "mappends correctly" $ do
            mappend [1,2,3,4] [5,6,7] `shouldBe` [1,2,3,4,5,6,7]
        it "calculates 10 with MySum" $ do
            ten `shouldBe` 10
        it "calculates 66 with MyProduct" $ do
            sixtySix `shouldBe` 66
