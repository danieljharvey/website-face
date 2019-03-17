module ReaderSpec where

import           Reader
import           Test.Hspec

-- spec :: IO ()
spec =
  describe "Reader" $ do
    it "Runs the most basic Reader" $
      runningIt `shouldBe` "Hello, Dog"
    it "Functor" $
      mapped `shouldBe` "Hello, Dog!!!!"
