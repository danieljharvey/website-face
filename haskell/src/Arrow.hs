{-# LANGUAGE Arrows #-}
module Arrow where

import           Control.Arrow
import           Control.Category
import           Prelude          hiding (id, (.))

newtype SimpleFunc a b
  = SimpleFunc { runF :: a -> b }

instance Arrow SimpleFunc where
  arr = SimpleFunc

  first (SimpleFunc f) = SimpleFunc (mapFst f)
    where mapFst g (a,b) = (g a, b)

  second (SimpleFunc f) = SimpleFunc (mapSnd f)
    where mapSnd g (a,b) = (a, g b)

instance Category SimpleFunc where
  (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)

  id = arr id

data Animal
  = Cat
  | Dog
  | Horse
  | Other
  deriving (Show, Eq, Ord)

readAnimal :: String -> Animal
readAnimal "cat"   = Cat
readAnimal "dog"   = Dog
readAnimal "horse" = Horse
readAnimal _       = Other

cycleAnimal :: Animal -> Animal
cycleAnimal Cat   = Dog
cycleAnimal Dog   = Horse
cycleAnimal Horse = Cat
cycleAnimal Other = Other

showAnimal :: SimpleFunc String Animal
showAnimal = arr readAnimal

f :: SimpleFunc Int Int
f = arr (`div` 2)

g :: SimpleFunc Int Int
g = arr (\x -> x*3 + 1)

h' :: SimpleFunc Int Int
h' = proc x -> do
  fx <- f -< x
  gx <- g -< x
  returnA -< (fx + gx)

hOutput' :: Int
hOutput' = runF h' 1
