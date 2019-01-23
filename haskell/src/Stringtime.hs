{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Stringtime where

import Data.Kind

data Stuff
  = Stuff
      { stName :: String
      , stAge  :: Integer
      }

type Template = Stuff -> String

parts :: [Template]
parts = [ const "<html><head>"
        , (\s -> "<title>" <> stName s <> "</title>")
        , const "</head>"
        , const "<body>"
        , (\s -> "<h1>Let's have a nice time with the number " <> show (stAge s) <> "!!!</h1>")
        , const "</body></html>"
        ]

render :: Stuff -> [Template] -> String
render s ts
  = concat $ fmap apply ts
  where
    apply t = t s

--

data SwitchState = On | Off
  deriving (Enum, Bounded, Show) 

data FocusState = NotFocused | Focused | Blurred
  deriving (Enum, Bounded, Show)

-- bum

data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

instance Show (HList '[]) where
  show _ = ""

instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
  show (HCons x xs) = ("!! " ++ show x ++ " !!") ++ show xs

--

class CartesianProductino (as :: [Type]) where
  cartesianLaddo :: [HList as]  

instance CartesianProductino '[] where
  cartesianLaddo = []

instance {-# OVERLAPPING #-} (Bounded a, Enum a) => CartesianProductino '[a] where
  cartesianLaddo = fmap (\b -> HCons b HNil) [minBound..maxBound]

instance (CartesianProductino as, Bounded a, Enum a) => CartesianProductino (a ': as) where
  cartesianLaddo = do
    x <- [minBound..maxBound]
    xs <- cartesianLaddo
    pure (HCons x xs)


  {-
data Dict (c :: Constraint) where
  Dict :: c => Dict c

f :: Dict (Eq Int)
f = Dict

isEqual :: (Typeable a, Typeable b) => a -> b -> Maybe (Dict (a ~ b))
isEqual = eqTypeRep

data Renderable where
  Make :: (Typeable a, Renderable a) => a -> Renderable

diff :: Renderable -> Renderable -> Patch
diff x y = case eqTypeRep x y of
  Nothing -> [ delete x, add y]
  Just Refl -> calculatePatch x y

calculatePatch :: a -> a -> ...

data x :~: y where
    Refl :: x :~: x

data Title
  = Title
      { switch :: SwitchState
      , focus :: FocusState
      }
  deriving Generic -- !!!!!!!!

class TypeToHList (x :: Type) (xs :: [Type]) where
  convert :: x -> HList xs

generate :: (Generic x, TypeToList x xs, CartesianProductino xs) => [x]

type TitleStyle = HList '[SwitchState, FocusState]

blah :: TitleStyles -> CSS
blah (HCons switch (HCons focus _)) = undefined

-}

