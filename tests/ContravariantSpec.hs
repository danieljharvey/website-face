module ContravariantSpec where

import           Control.Exception (evaluate)
import           Contravariant
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Contravariant" $ do
    it "overThree" $ do
      (getPreddy overThree $ 4) `shouldBe` True
    it "notOverThree" $ do
      (getPreddy overThree $ 3) `shouldBe` False
    it "nameLength" $ do
      nameLength "" `shouldBe` 0
    it "nameLength 2" $ do
      nameLength "Dog" `shouldBe` 3
    it "nameIsOverThree 1" $ do
      nameIsOverThree "Lou" `shouldBe` False
    it "nameIsOverThree 2" $ do
      nameIsOverThree "Doug" `shouldBe` True
