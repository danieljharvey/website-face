{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
module Free where

import           Control.Monad.Free
import qualified Control.Monad.State       as St
import           Control.Monad.Writer.Lazy
import           Network.Curl
import           Prelude
import           System.Exit

data ConsoleF a
  = Write String a
  | Read (String -> a)

instance Functor ConsoleF where
    fmap f (Write s next) = Write s (f next)
    fmap f (Read next)    = Read (f . next)

type Console a = Free ConsoleF a

fWrite :: String -> Console ()
fWrite str = liftF $ Write str ()

fRead :: Console String
fRead = liftF $ Read id

fStop :: Console ()
fStop = pure ()

consoleProg :: Console ()
consoleProg = do
    fWrite "What is your name?"
    a <- fRead
    fWrite $ "Sure? " ++ a
    b <- fRead
    fWrite "Great."

interpretIO :: Console a -> IO a
interpretIO
  = foldFree interpret
  where
    interpret :: ConsoleF a -> IO a
    interpret prog'
      = case prog' of
          Write s a -> Prelude.putStrLn s >> pure a
          Read a    -> a <$> Prelude.getLine

interpretWrite :: Console a -> Writer [String] a
interpretWrite = foldFree interpret
  where
    interpret prog'
      = case prog' of
          Write s a -> do
            _ <- tell [s]
            pure a
          Read a -> do
            _ <- tell ["wait for input"]
            pure (a "input")

data ReducerF s a
  = Modify (s -> s) a
  | Get (s -> a)
  | Fetch String (String -> a)
  deriving (Functor)

type Reducer s a
  = Free (ReducerF s) a

data State
  = State { string  :: Maybe String
          , url     :: String
          , loading :: Bool
          }
  deriving (Eq, Ord, Show)

modify :: (s -> s) -> Reducer s ()
modify f = liftF $ Modify f ()

fetch :: String -> Reducer s String
fetch url = liftF $ Fetch url id

get :: Reducer s s
get = liftF $ Get id

fetchAction :: Reducer State ()
fetchAction = do
  modify (\s
    -> s { loading = True })
  state <- get
  str <- fetch (url state)
  modify (\s
    -> s { loading = False, string = Just str })

interpretState :: Reducer State a -> St.State State a
interpretState = foldFree interpret
  where
    interpret prog'
      = case prog' of
          Modify f next -> do
            St.modify f
            pure next
          Get next ->
            next <$> St.get
          Fetch url next ->
            pure (next "test item")

interpretStateIO :: Reducer State a -> St.StateT State IO a
interpretStateIO = foldFree interpret
  where
    interpret prog'
      = case prog' of
        Modify f next -> do
          St.modify f
          pure next
        Get next ->
          next <$> St.get
        Fetch url next -> do
          (_, s) <- lift $ curlGetString url []
          pure (next s)

-- snd <$> runStateT (interpretStateIO fetchAction) initialState

initialState :: State
initialState
  = State { string = Nothing
          , url = "http://internetisverymuchmybusiness.com"
          , loading = False
          }
