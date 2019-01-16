module EffectAndAff where

import Prelude (Unit, bind, discard, pure, unit)
import Data.Show

import Effect (Effect)
import Effect.Timer

newtype TimeReturn = TimeReturn TimeoutId

callback :: forall a. a -> (a -> Effect Unit) -> Effect Unit
callback a cb = cb a

instance showTimeoutId :: Show TimeReturn where
  show _ = "TIMEOUTID"

callbackWithDelay :: forall a. a -> (a -> Effect Unit) -> Effect TimeReturn
callbackWithDelay a cb = do
  timeoutId <- (setTimeout 1000 (cb a))
  pure (TimeReturn timeoutId)
