module NewtypesSpec where

import           Control.Exception (evaluate)
import           Newtypes
import           Test.Hspec
import           Test.QuickCheck   hiding (NonEmpty)

-- spec :: IO ()
spec = describe "Newtypes" $ do
    it "Makes Frank" $
        getDog frank `shouldBe` "Frank"
    it "wraps and unwraps" $
        itsTheSame `shouldBe` True

