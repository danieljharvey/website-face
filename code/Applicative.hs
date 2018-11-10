module Applicative where

import           Data.List   (concat, intersperse)
import           Data.Monoid ((<>))

data CalcFace a = CalcFace [String] a deriving (Eq)

instance (Show a) => Show (CalcFace a) where
    show (CalcFace names a) = concat parts ++ " equals " ++ show a where
        parts = intersperse " " names

instance Functor CalcFace where
   fmap f (CalcFace names a) = CalcFace names $ f a

one :: CalcFace Int
one = CalcFace ["1"] 1

two :: CalcFace Int
two = fmap (+1) one

addOne :: CalcFace (Int -> Int)
addOne = CalcFace ["add 1"] (+1)

instance Applicative CalcFace where
    (CalcFace operation f) <*> (CalcFace value a) = CalcFace newNames $ f a where
        newNames = value <> operation
    pure = CalcFace []

oneAddOne :: CalcFace Int
oneAddOne = addOne <*> one

oneAddOneAddOne :: CalcFace Int
oneAddOneAddOne = addOne <*> oneAddOne

instance Monad CalcFace where
    (CalcFace _ a) >>= k = k a

addThreeMonadically :: Int -> CalcFace Int
addThreeMonadically i = CalcFace ["add 3"] $ i + 3

oneAddThreeMonadically :: CalcFace Int
oneAddThreeMonadically = one >>= addThreeMonadically

oneAddThreeAddThreeMonadically :: CalcFace Int
oneAddThreeAddThreeMonadically = one >>= addThreeMonadically
                                     >>= addThreeMonadically

