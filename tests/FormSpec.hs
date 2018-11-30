module FormSpec where

import           Control.Exception (evaluate)
import           Form
import           Test.Hspec
import           Test.QuickCheck   hiding (NonEmpty)

-- spec :: IO ()
spec =
    describe "Form" $
        it "Form returns a result" $
            result twoNumsForm (1,2) `shouldBe` 3

