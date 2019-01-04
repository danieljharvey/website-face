module Alternative where

import Control.Applicative

data Perhaps a = Yeah a | Nope deriving (Eq, Show)

instance Functor Perhaps where
  fmap f (Yeah a) = Yeah (f a)
  fmap _ _        = Nope

instance Applicative Perhaps where
  pure = Yeah
  (Yeah f) <*> (Yeah a) = Yeah (f a)

instance Alternative Perhaps where
    empty             = Nope
    Nope   <|> Nope   = Nope
    x      <|> Nope   = x
    Nope   <|> x      = x
    Yeah x <|> Yeah y = Yeah x

-- test functions

first :: [a] -> Perhaps a
first []    = Nope
first (a:_) = Yeah a

second :: [a] -> Perhaps a
second (_:b:_) = Yeah b
second _       = Nope

-- combining them with <|>

getSecondOrFirst :: [a] -> Perhaps a
getSecondOrFirst as = second as <|> first as

-- answers

nah :: Perhaps Int
nah = getSecondOrFirst []
-- nah == Nope

fallback :: Perhaps Int
fallback = getSecondOrFirst [1]
-- fallback == Yeah 1

found :: Perhaps Int
found = getSecondOrFirst [1,2]
-- found == Yeah 2
