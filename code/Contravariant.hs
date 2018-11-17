module Contravariant where

import Data.Functor.Contravariant

newtype Preddy a = Preddy { getPreddy :: a -> Bool }

overThree :: Preddy Int
overThree = Preddy (\a -> a > 3)

isOverThree :: Int -> Bool
isOverThree i = getPreddy overThree $ i
-- isOverThree 2 == False
-- isOverThree 4 == True

instance Contravariant Preddy where
  contramap f (Preddy p) = Preddy (p . f)

nameLength :: String -> Int
nameLength "" = 0
nameLength (x: xs) = 1 + nameLength xs

nameLengthOverThree :: Preddy String
nameLengthOverThree = contramap nameLength overThree

nameIsOverThree :: String -> Bool
nameIsOverThree str = getPreddy nameLengthOverThree str
-- nameIsOverThree "Lou" == False
-- nameIsOverThree "Doug" == True
