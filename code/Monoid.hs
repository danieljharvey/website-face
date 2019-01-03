module Monoid where

-- List bits

listCombine :: [Int] -> [Int] -> [Int]
listCombine a b = a ++ b

bothLists :: [Int]
bothLists = listCombine [1,2,3] [4,5,6]
-- bothLists == [1,2,3,4,5,6]

emptyList :: [Int]
emptyList = []

addNothing :: [Int]
addNothing = listCombine [1,2,3] emptyList
-- addNothing == [1,2,3]

{-
instance Semigroup [a] where
  a <> b = listCombine a b

instance Monoid [a] where
  mempty = []
-}

data MyMaybe a = Yeah a | Nope



-- Sum Monoid

newtype MySum a = MySum {
    getMySum :: a
}

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

instance (Num a) => Monoid (MySum a) where
    mempty = MySum 0

ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2

-- Product Monoid

newtype MyProduct a = MyProduct {
    getMyProduct :: a
}

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct a <> MyProduct b = MyProduct (a * b)

instance (Num a) => Monoid (MyProduct a) where
    mempty = MyProduct 1

sixtySix :: Int
sixtySix = getMyProduct $ MyProduct 11 <> MyProduct 2 <> MyProduct 3
