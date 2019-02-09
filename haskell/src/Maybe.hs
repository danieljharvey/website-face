module Maybe where

import Prelude hiding (Maybe(..))
import Control.Applicative
import Control.Monad.Fail
import Control.Monad
import Control.Monad.Zip
import Control.Monad.Trans.Class

data Maybe a = Just a | Nothing
  deriving (Eq, Ord, Show)

-- If we have a value, run the function on it
-- if we don't - do nothing
instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing

-- pure = default for datatype.
-- we assume success so Just
-- <*> is for combining functions that are also in Maybe
instance Applicative Maybe where
  pure a = Just a
  (Just f) <*> (Just a) = Just (f a)
  _        <*> _        = Nothing

-- if we get two wrapped maybes, make it into one
-- if we start with Nothing, do nothing else
instance Monad Maybe where
  (Just a) >>= k   = k a
  Nothing  >>= _   = Nothing

-- concatenation of things - the things must themselves be combinable
instance (Semigroup a) => Semigroup (Maybe a) where
  (Just a) <> (Just b) = Just (a <> b)
  Nothing  <> a        = a
  a        <> Nothing  = a

-- empty value is nothing as appending nothing to anything does not change it
instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing

-- think of this as combining a list that may have one or zero items
instance Foldable Maybe where
  foldr _ a Nothing  = a
  foldr f a (Just b) = f b a

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

-- Our MaybeT monad transformer
newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }



