module Foldable where

added :: Int
added = foldr (\a b -> a + b) 0 [1,2,3,4]
-- added = 10

maxNo :: Int
maxNo = foldr (\a b -> if a > b then a else b) 0 [1,2,3,4]
-- maxNo = 4

-- let's make a new type
newtype MySum a = MySum { getMySum :: a }

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

instance (Num a) => Monoid (MySum a) where
    mempty = MySum 0

addTwo :: Int
addTwo = getMySum $ foldMap MySum [1,2,3,4]
-- addTwo = 10
