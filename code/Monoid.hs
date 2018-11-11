module Monoid where

strConcat :: String -> String -> String
strConcat a b = a ++ b

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f def []       = def
myFoldr f def (x : xs) = x `f` myFoldr f def xs

output1 :: String
output1 = myFoldr strConcat "" ["hot","dog","frog"]

output2 :: String
output2 = myFoldr strConcat "" []

output3 :: String
output3 = myFoldr strConcat "sorry, no value" []

output4 :: String
output4 = myFoldr strConcat "sorry, no value" ["hot","dog","frog"]

{-
instance Monoid [a] where
    mappend a b = a <> b
    mempty = []
-}

newtype MySum a = MySum {
    getMySum :: a
}

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

instance (Num a) => Monoid (MySum a) where
    mempty = MySum 0

ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2


newtype MyProduct a = MyProduct {
    getMyProduct :: a
}

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct a <> MyProduct b = MyProduct (a * b)

instance (Num a) => Monoid (MyProduct a) where
    mempty = MyProduct 1

sixtySix :: Int
sixtySix = getMyProduct $ MyProduct 11 <> MyProduct 2 <> MyProduct 3

data MyMaybe a = Yeah a | Nope
