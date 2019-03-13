module Comonad where

import           Control.Comonad.Store
import           Data.List             (isInfixOf)
import           Data.Monoid

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

{- yeah -}

type Grid = [[Int]]

startGrid :: Grid
startGrid = [ [0,0,1,0,0]
            , [0,0,1,0,0]
            , [1,1,1,1,1]
            , [0,0,1,0,0]
            , [0,0,1,0,0]
            ]

type Point = (Int, Int)

startPoint :: Point
startPoint = (2,2)

getGridItem :: Grid -> Point -> Int
getGridItem grid (x,y) = col !! x' where
  col = grid !! y'
  x' = x `mod` 5
  y' = y `mod` 5

startStore :: Store Point Int
startStore
  = store (getGridItem startGrid) startPoint

firstItem :: Int
firstItem = extract startStore
-- firstItem == 1

-- adds up all of the items around our item
nextStep :: Store Point Int -> Int
nextStep store
  = foldr (+) 0 [ change (-1) (-1)  , change 0 (-1) , change 1 (-1)
                , change (-1) 0   , change 0 0  , change 1 0
                , change (-1) 1   , change 0 1  , change 1 1
                ]
    where
      change :: Int -> Int -> Int
      change x y
        = peeks (\(a,b) -> (x + a, y + b)) store

-- our startStore, after nextStep has been run over each item
endStore :: Store Point Int
endStore = extend nextStep startStore

secondItem :: Int
secondItem = extract endStore
-- secondItem == 5

thirdItem :: Int
thirdItem = peek (0,1) endStore
-- thirdItem == 3

fourthItem :: Int
fourthItem = peek (0,0) endStore
-- fourthItem == 0
