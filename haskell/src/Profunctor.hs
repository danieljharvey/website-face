module Profunctor where

import           Data.Profunctor

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
