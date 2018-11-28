module Newtypes where

newtype Dog a = Dog { getDog :: a } deriving (Show)

frank :: Dog String
frank = Dog "Frank"
-- frank == Dog "Frank"

name :: String
name = getDog frank
-- name == "Frank"

itsTheSame :: Bool
itsTheSame = "Frank" == getDog (Dog "Frank")

newtype Cat a = Cat { getCat :: a } deriving (Show)

mimas :: Cat String
mimas = Cat "Mimas"
-- mimas == Cat "Mimas"

data Hand = Hand

class Pettable a where
    (<>|<>) :: Hand -> a -> String

instance (Show a) => Pettable (Dog a) where
    Hand <>|<> (Dog a) = show a ++ " woofs or something"

instance (Show a) => Pettable (Cat a) where
    Hand <>|<> (Cat a) = show a ++ " miaows or similar"


