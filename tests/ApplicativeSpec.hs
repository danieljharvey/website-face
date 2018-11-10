module ApplicativeSpec where

import           Applicative
import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

-- spec :: IO ()
spec = do
  describe "Applicative" $ do
    it "Maps correctly" $ do
      two `shouldBe` CalcFace ["1"] 2
    it "Applies correctly" $ do
      oneAddOneAddOne `shouldBe` CalcFace ["1", "add 1","add 1"] 3
    it "Monadically applies" $ do
      oneAddThreeAddThreeMonadically `shouldBe` CalcFace ["add 3"] 7
    it "Quickchecks functor identity" $ do
      quickCheck calcFaceFunctorIdentity
    it "Quickchecks applicative identity" $ do
      quickCheck calcFaceApplicativeIdentity


instance Arbitrary a => Arbitrary (CalcFace a) where
  arbitrary = do
    a <- arbitrary
    xs <- arbitrary
    return $ CalcFace xs a

calcFaceFunctorIdentity :: CalcFace String -> Bool
calcFaceFunctorIdentity cf = fmap id cf == cf

calcFaceApplicativeIdentity :: CalcFace String -> Bool
calcFaceApplicativeIdentity v = (pure id <*> v) == v
