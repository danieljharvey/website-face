---
title: Typeclasses - Profunctor
tags: haskell, typeclasses
---

Hello.

module Profunctor where

import Data.Profunctor

{-

import Data.Profunctor

Data.Profunctor> :i Profunctor

class Profunctor (p :: _ -> _ -> \*) where
dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
lmap :: (a -> b) -> p b c -> p a c
rmap :: (b -> c) -> p a b -> p a c
{-# MINIMAL dimap | lmap, rmap #-}

-}

newtype FuncBox b c
= FuncBox { runFuncBox :: b -> c }

instance Profunctor FuncBox where
dimap before after (FuncBox f)
= FuncBox (after . f . before)

data Animal = Horse | Dog | Cat
deriving (Show)

repeatEgg :: Int -> [String]
repeatEgg s
= replicate s "Egg"

length' :: FuncBox String Int
length' = FuncBox length

leftMapped :: FuncBox Animal Int
leftMapped
= lmap show length'

rightMapped :: FuncBox String String
rightMapped
= rmap show length'

dimapped :: FuncBox Animal [String]
dimapped
= dimap show repeatEgg length'

one :: Int
one = runFuncBox length' "horses"
-- one == 6

two :: [String]
two = runFuncBox dimapped Dog
-- two == ["Egg", "Egg", "Egg"]

three :: Int
three = runFuncBox leftMapped Horse
-- three == 5

four :: String
four = runFuncBox rightMapped "Dog"
-- four == "3"
