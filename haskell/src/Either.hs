module Either where

import Prelude hiding (Either(..))
import Control.Applicative
import Control.Monad.Fail
import Control.Monad hiding (fail)
import Control.Monad.Zip

data Either a b
  = Left a
  | Right b
  deriving (Eq, Ord, Show)

instance Functor (Either a) where
  fmap f (Right b) = Right (f b)
  fmap _ (Left a)  = Left a

-- pure = default for datatype.
-- we assume success so Right
-- <*> is for combining functons that are also in Either
instance Applicative (Either a) where
  pure a = Right a
  (Right f) <*> (Right a) = Right (f a)
  (Right f) <*> (Left  a) = Left a
  (Left a)  <*> _         = Left a

-- if we get two wrapped eithers, make it into one
-- if we start with Left, do nothing else
instance Monad (Either a) where
  Right a >>= k   = k a
  Left e  >>= _   = Left e

-- concatenation of things - the things must themselves be combinable
instance (Semigroup a) => Semigroup (Either e a) where
  (Right a) <> (Right b) = Right (a <> b)
  (Left a)  <> (Left b)  = Left a
  (Left a)  <> b         = b
  a         <> (Left b)  = a

-- since Left needs to have a value inside of it, we can only have mempty if the e has a mempty too
instance (Semigroup a, Monoid e) => Monoid (Either e a) where
  mempty = Left mempty

-- think of this as combining a list that may have one or zero items
instance Foldable (Either e) where
  foldr _ a (Left e)  = a
  foldr f a (Right b) = f b a

  {-
instance Alternative Maybe where
  empty                 = Nothing
  (Just a) <|> _        = Just a
  Nothing  <|> (Just b) = Just b
  Nothing  <|> Nothing  = Nothing

instance Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just a) = fmap Just (f a)

-- provides a general purpose way of failing a computation
instance MonadFail Maybe where
  fail _ = Nothing

-- it's just Alternative again!
instance MonadPlus Maybe where
  mzero = Nothing

  mplus (Just a) _ = Just a
  mplus (Nothing) (Just b) = Just b
  mplus _ _ = Nothing  

-- a monad generalising zipLists (combining two sets of things into one)
instance MonadZip Maybe where
  mzipWith = liftA2

-}
