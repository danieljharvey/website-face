module LensesSpec where

import           Control.Exception (evaluate)
import           Control.Lens
import           Lenses
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Manually getting data" $ do
    it "gets port" $
      getPort appData `shouldBe` 8080
    it "increments the port" $
      getPort (incrementPort appData) `shouldBe` 8081
    it "sets the port" $
      getPort (setPort 10 appData) `shouldBe` 10
    it "gets 'count' when it's Right" $
      getCountInt appData `shouldBe` Just 100
    it "doesn't get 'count' when it's Right" $
      getCountError appData `shouldBe` Nothing
    it "sets and fetches a new Right" $
      getCountInt (setCountInt 666 appData) `shouldBe` Just 666
    it "sets and fetches a new Left" $
      getCountError (setCountError "Horses" appData) `shouldBe` Just "Horses"
  describe "The same with sweet, sweet Lens" $ do
    it "gets port with lens" $
      view fullPortLens appData `shouldBe` 8080
    it "increments the port with lens" $
      getPort (over fullPortLens (+1) appData) `shouldBe` 8081
    it "sets the port with lens" $
      getPort (set fullPortLens 10 appData) `shouldBe` 10
    it "gets 'count' when it's Right with lens" $
      (view countIntPrism appData) `shouldBe` Just 100

