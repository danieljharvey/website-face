module Semigroup where

data NonEmpty a = NonEmpty a [a] deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
    (NonEmpty a as) <> (NonEmpty b bs) = NonEmpty a $ as <> [b] <> bs

toList :: NonEmpty a -> [a]
toList (NonEmpty a as) = [a] <> as

first :: NonEmpty Int
first = NonEmpty 1 [2,3,4]

second :: NonEmpty Int
second = NonEmpty 5 [6,7,8]

third :: NonEmpty Int
third = first <> second
