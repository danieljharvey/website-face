module Functor where

import           Data.Char

data Perhaps a = Yeah a | Nah deriving (Eq)

instance Functor Perhaps where
    fmap _ Nah      = Nah
    fmap f (Yeah a) = Yeah (f a)

nope :: Perhaps String
nope = Nah

john :: Perhaps String
john = Yeah "John"

exclaim :: String -> String
exclaim str = str ++ "!!!!!!!!!!!!"
-- exclaim "Horse" == "Horse!!!!!!!!!!!!"

capitalise :: String -> String
capitalise = fmap toUpper
-- capitalise "Horse" == "HORSE"

veryJohn :: Perhaps String
veryJohn = fmap exclaim john
-- veryJohn == Yeah "John!!!!!!!!!!!!"

stillNope :: Perhaps String
stillNope = fmap exclaim nope
-- stillNope = Nah

returnA :: a -> a
returnA a = a
-- this is also called id in the Prelude

identityLaw :: Bool
identityLaw = john == fmap id john
-- always does nothing

compositionLaw :: Bool
compositionLaw = fmap (capitalise . exclaim) john == (fmap capitalise (fmap exclaim john))
-- either way of doing this ends up the same
