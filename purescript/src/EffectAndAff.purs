module EffectAndAff where

import Prelude (Unit, const, bind, discard, pure, unit, ($))
import Data.Show

import Effect (Effect)
import Effect.Timer
import Effect.Console

newtype TimeReturn = TimeReturn TimeoutId

callback :: forall a. a -> (a -> Effect Unit) -> Effect Unit
callback a cb = cb a

instance showTimeoutId :: Show TimeReturn where
  show _ = "TIMEOUTID"

callbackWithDelay :: forall a. a -> (a -> Effect Unit) -> Effect TimeReturn
callbackWithDelay a cb = do
  timeoutId <- (setTimeout 1000 (cb a))
  pure (TimeReturn timeoutId)

manyCallbacks :: forall a. a -> (a -> Effect Unit) -> Effect Unit
manyCallbacks a cb
  = do
     log "1"
     _ <- callbackWithDelay a 
      (\b -> do 
         log "2" 
         _ <- callbackWithDelay b 
          (\c -> do
              log "3"
              _ <- cb c
              pure unit) 
         pure unit)
     pure unit
-- returns
-- 1
-- unit
-- 2
-- 3
