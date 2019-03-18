module MonadSpec where

import           Monad
import           Test.Hspec

-- spec :: IO ()
spec =
  describe "Monad" $ do
    it "Empty test" $
      1 `shouldBe` 1

