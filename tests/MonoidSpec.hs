module MonoidSpec where

import           Control.Exception (evaluate)
import           Monoid
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = describe "Monoids" $ do
    it "strConcat" $
        strConcat "Spruce" "Goose" `shouldBe` "SpruceGoose"
    it "myFoldr" $
        myFoldr strConcat "" ["hot","dog","frog"] `shouldBe` "hotdogfrog"
    it "output1" $
        output1 `shouldBe` "hotdogfrog"
    it "output2" $
        output2 `shouldBe` ""
    it "output3" $
        output3 `shouldBe` "sorry, no value"
    it "output4" $
        output4 `shouldBe` "hotdogfrogsorry, no value"
    it "mappends correctly" $
        mappend [1,2,3,4] [5,6,7] `shouldBe` [1,2,3,4,5,6,7]
    it "calculates 10 with MySum" $
        ten `shouldBe` 10
    it "calculates 66 with MyProduct" $
        sixtySix `shouldBe` 66
