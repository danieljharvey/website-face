module ArrowSpec where

import           Arrow
import           Test.Hspec

-- spec :: IO ()
spec =
  describe "Arrow" $
    it "Seems OK" $
      "1" `shouldBe` "1"
