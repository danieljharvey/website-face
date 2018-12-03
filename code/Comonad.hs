module Comonad where

import           Control.Comonad.Store
import           Data.List             (isInfixOf)

data Which = This | That | Who deriving (Show, Eq)

getStuff :: Which -> String
getStuff This = "this thing"
getStuff That = "that other thing"
getStuff Who  = "who the fuck?"

stuffStore :: Store Which String
stuffStore = store getStuff This

(f,a) = runStore stuffStore

output :: String
output = f a

other :: String
other = f That

currentPosition :: Which
currentPosition = pos stuffStore

somethingElse :: String
somethingElse = peek That stuffStore

invert :: Which -> Which
invert This = That
invert That = Who
invert Who  = This

swapped :: String
swapped = peeks invert stuffStore

focusedOnThat :: Store Which String
focusedOnThat = seek That stuffStore

itsThat :: Which
itsThat = pos focusedOnThat

nowItsWho :: Which
nowItsWho = pos $ seeks invert focusedOnThat

-- experiment :: Functor f => (s -> f s) -> w a -> f a

thisOrThat :: Which -> Maybe Which
thisOrThat Who = Nothing
thisOrThat w   = Just w

-- experiment is 'functor map over key to get fuc
what = experiment thisOrThat stuffStore

otherWhat = experiment thisOrThat $ seeks invert stuffStore

otherOtherWhat = experiment thisOrThat $ seeks invert $ seeks invert stuffStore

{-
 oh my god what are we even doing, this seems so much work,

 lets try a more interesting key maybe?
-}

data Slice = First | Middle | Last deriving (Eq, Show)

type Location = (Slice, Slice)

data BattenType = Pink | Other deriving (Eq, Show)

getBattenType :: Location -> BattenType
getBattenType (x, y) = if x == y then Pink else Other


