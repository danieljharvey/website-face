module MaybeSpec where

import           Control.Monad.Fail
import           Control.Applicative
import           Prelude hiding (Maybe(..), fail)
import           Maybe
import           Test.Hspec

-- spec :: IO ()
spec =
  describe "Maybe" $ do
    it "Maybe functor with Just" $
      fmap (+1) (Just (1 :: Int)) `shouldBe` Just 2
    it "Maybe functor with Nothing" $
      fmap (+1) Nothing `shouldBe` Nothing
    it "Maybe applicative (pure)" $
      pure 1 `shouldBe` Just 1
    it "Maybe applicative (<*>)" $
      Just (+1) <*> Just 1 `shouldBe` Just 2
    it "Maybe applicative <*> with Nothing" $
     Just (+1) <*> Nothing `shouldBe` Nothing
    it "Maybe monad with Just" $
      (Just 1 >>= (\a -> Just a)) `shouldBe` Just 1
    it "Maybe monad with Nothing" $
      (Nothing >>= (\a -> Just a)) `shouldBe` (Nothing :: Maybe Int)
    it "Maybe Semigroup with Just" $
      Just [1,2,3] <> Just [4,5,6] `shouldBe` Just [1,2,3,4,5,6]
    it "Maybe Semigroup with Nothing" $
      Nothing <> Just [1,2,3] `shouldBe` Just [1,2,3]
    it "Maybe Semigroup with other Nothing" $
      Just [1,2,3] <> Nothing `shouldBe` Just [1,2,3]
    it "Maybe Semigroup with all Nothing" $
      Nothing <> Nothing `shouldBe` (Nothing :: Maybe [Int])
    it "Maybe Monoid" $
      mempty `shouldBe` (Nothing :: Maybe [Int])
    it "Foldable Maybe with Nothing" $
      (foldr (+) 1 Nothing) `shouldBe` 1
    it "Foldable Maybe with Just" $
      (foldr (+) 1 (Just 10)) `shouldBe` 11
    it "Alternative ending in nothing" $
      (Nothing <|> Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "Alternative first" $
      (Just 1 <|> Just 2) `shouldBe` Just 1
    it "Alternative second" $
      (Nothing <|> Just 2) `shouldBe` Just 2
    it "Traverses list Nothing" $
      (traverse (\a -> [a,a]) (Nothing :: Maybe Int)) `shouldBe` [Nothing]
    it "Traverses list and Just 1" $
      (traverse (\a -> [a,a]) $ Just 10) `shouldBe` [Just 10, Just 10]
    it "Monad fail returns Nothing" $ do
      (Just "yes" >>= fail) `shouldBe` (Nothing :: Maybe String)
