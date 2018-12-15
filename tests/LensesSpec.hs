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
  describe "Prisms" $ do
    it "Gets the string" $
      preview dogStringPrism dogString `shouldBe` Just "Dog Name"
    it "Gets no string" $
      preview dogIntPrism dogString `shouldBe` Nothing
    it "Gets the int" $
      preview dogIntPrism dogInt `shouldBe` Just 100
    it "Gets no int" $
      preview dogStringPrism dogInt `shouldBe` Nothing
    it "Changes name on dogString" $
      preview dogStringPrism (set dogStringPrism "Bruce" dogString) `shouldBe` Just "Bruce"
    it "Can't change name on dogInt" $
      preview dogStringPrism (set dogStringPrism "Bruce" dogInt) `shouldBe` Nothing
    it "Changes age on dogInt" $
      preview dogIntPrism (set dogIntPrism 27 dogInt) `shouldBe` Just 27
    it "Can't change age on dogString" $
      preview dogIntPrism (set dogIntPrism 27 dogString) `shouldBe` Nothing
  describe "The same with sweet, sweet Lens" $ do
    it "gets port with lens" $
      view fullPortLens appData `shouldBe` 8080
    it "increments the port with lens" $
      getPort (over fullPortLens (+1) appData) `shouldBe` 8081
    it "sets the port with lens" $
      getPort (set fullPortLens 10 appData) `shouldBe` 10
    it "gets 'count' when it's Right with lens" $
      (preview fullCountInt appData) `shouldBe` Just 100
    it "doesn't get 'count' when it's Right with lens" $
      (preview fullCountError appData) `shouldBe` Nothing
    it "sets and fetches a new Right with lens" $
      getCountInt (set fullCountInt 666 appData) `shouldBe` Just 666
    it "can't set a new Left with the prism" $
      getCountInt (set fullCountError "Horses" appData) `shouldBe` Just 100
    it "must change the sum further up the tree with lens" $
      getCountError (set countLens (Left "Horses") appData) `shouldBe` Just "Horses"
