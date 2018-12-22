module Traversal where

import Data.Monoid
import Data.Maybe
import Data.Validation

data MyTree a = Leaf a | Branch (MyTree a) (MyTree a) deriving (Show, Eq)

sampleTree :: MyTree Int
sampleTree = Branch (Branch (Leaf 2) (Leaf 3)) (Branch (Leaf 5) (Leaf 2))

instance Foldable MyTree where
  foldMap f (Branch l r) = (foldMap f l) <> (foldMap f r)
  foldMap f (Leaf a) = f a

sampleTreeTotal :: Int
sampleTreeTotal = getSum $ foldMap Sum sampleTree
-- sampleTreeTotal == 10

maybeTree :: MyTree (Maybe Int)
maybeTree = Branch (Branch (Leaf $ Just 2) (Leaf $ Just 3)) (Branch (Leaf $ Just 5) (Leaf $ Just 2))

instance Functor MyTree where
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  fmap f (Leaf a) = Leaf (f a)

{-
*Main> :i Traversable
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
-}

instance Traversable MyTree where
  traverse f (Branch l r) = Branch <$> (traverse f l) <*> (traverse f r)
  traverse f (Leaf a) = Leaf <$> f a

justTree :: Maybe (MyTree Int)
justTree = traverse id maybeTree
-- justTree == Just (Branch (Branch (Leaf 2) (Leaf 3)) (Branch (Leaf 5) (Leaf 2)))

anotherMaybeTree :: MyTree (Maybe Int)
anotherMaybeTree = Branch (Branch (Leaf Nothing) (Leaf $ Just 3)) (Branch (Leaf $ Just 5) (Leaf $ Just 2))

nothingTree :: Maybe (MyTree Int)
nothingTree = traverse id anotherMaybeTree
-- nothingTree == Nothing

-- traverse id = sequence
listTree :: MyTree ([Int])
listTree = Branch (Leaf [1,2]) (Leaf [3,4])

invertedListTree :: [MyTree Int]
invertedListTree = traverse id listTree
{-
invertedListTree ==
  [ Branch (Leaf 1) (Leaf 3)
  , Branch (Leaf 1) (Leaf 4)
  , Branch (Leaf 2) (Leaf 3)
  , Branch (Leaf 2) (Leaf 4)
  ]
-}

reversedListTree :: [MyTree Int]
reversedListTree = traverse reverse listTree
{-
reversedListTree ==
  [ Branch (Leaf 2) (Leaf 4)
  , Branch (Leaf 2) (Leaf 3)
  , Branch (Leaf 1) (Leaf 4)
  , Branch (Leaf 1) (Leaf 3)
]
-}

eitherTree :: MyTree (Either String Int)
eitherTree = Branch (Leaf $ Right 100) (Leaf $ Right 200)

rightTree :: Either String (MyTree Int)
rightTree = traverse id eitherTree
-- rightTree == Right (Branch (Leaf 100) (Leaf 200))

failsTree :: Either String (MyTree Int)
failsTree = traverse id $ Branch (Leaf $ Right 1) (Leaf $ Left "2")
-- "Left 2"

failsTree2 :: Either String (MyTree Int)
failsTree2 = traverse id $ Branch (Leaf $ Left "1") (Branch (Leaf $ Left "2") (Leaf $ Left "3"))
-- "Left 1"

-- validation!

validationTree :: MyTree (Validation [String] Int)
validationTree = Branch (Leaf $ Success 100) (Branch (Leaf $ Failure ["2"]) (Leaf $ Failure ["3"]))

collectFails :: Validation [String] (MyTree Int)
collectFails = traverse id validationTree
-- collectFails == Failure ["2","3"]
