module EffectAndAff where

import Prelude (Unit, bind, discard, pure, unit, ($), mempty)
import Data.Show
import Data.Either

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer
import Effect.Console
import Effect.Aff

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
              log "2"
              _ <- callbackWithDelay c
                (\d -> do
                  log "2"
                  _ <- callbackWithDelay d
                    (\e -> do
                      log "2"
                      _ <- callbackWithDelay e
                        (\f -> do
                          log "2"
                          _ <- callbackWithDelay f
                            (\g -> do
                              log "2"
                              _ <- callbackWithDelay g
                                (\h -> do
                                  log "2"
                                  _ <- callbackWithDelay h
                                    (\i -> do
                                      log "2"
                                      _ <- callbackWithDelay i
                                        (\j -> do
                                          log "3"
                                          _ <- cb j
                                          pure unit)
                                      pure unit)
                                  pure unit)
                              pure unit)
                          pure unit)
                      pure unit)
                  pure unit)
              pure unit)
          pure unit)
      pure unit

delayAff :: forall a. a -> Aff a
delayAff a = makeAff affCallback
  where
    affCallback :: (Either Error a -> Effect Unit) -> Effect Canceler
    affCallback success
      = do
          _ <- callbackWithDelay a (\b -> success (Right b))
          pure mempty
    
    makeCanceller :: TimeoutId -> Error -> Aff Unit
    makeCanceller timer
      = (\_ -> do
          liftEffect $ clearTimeout timer
          pure unit)

go :: forall a. (Show a) => a -> Effect Unit
go a = do
  runAff_ (\answer -> log (show answer)) (makeAffs a)

makeAffs :: forall a. a -> Aff a
makeAffs a = do
  b <- delayAff a
  liftEffect $ log "1"
  c <- delayAff b
  liftEffect $ log "2"
  d <- delayAff c
  liftEffect $ log "3"
  delayAff d
