{-# LANGUAGE FlexibleContexts #-}
module Free where

import           Control.Monad.Free
import           Control.Monad.Writer.Lazy
import           Prelude
import           System.Exit

data ConsoleF a
  = Write String a
  | Read (String -> a)

instance Functor ConsoleF where
    fmap f (Write s next) = Write s (f next)
    fmap f (Read next)    = Read (f . next)

fWrite :: String -> Free ConsoleF ()
fWrite str = liftF $ Write str ()

fRead :: Free ConsoleF String
fRead = liftF $ Read id

fStop :: Free ConsoleF ()
fStop = pure ()

consoleProg :: Free ConsoleF ()
consoleProg = do
    fWrite "What is your name?"
    a <- fRead
    fWrite $ "Sure? " ++ a
    b <- fRead
    fWrite "Great."

interpretIO :: Free ConsoleF a -> IO a
interpretIO (Pure r) = return r
interpretIO (Free (Write s next))
  = Prelude.putStrLn s >> interpretIO next
interpretIO (Free (Read next))
  = Prelude.getLine >>= interpretIO . next


interpretIO2 :: Free ConsoleF a -> IO a
interpretIO2
  = foldFree interpret
  where
    interpret :: ConsoleF a -> IO a
    interpret prog'
      = case prog' of
          Write s a -> Prelude.putStrLn s >> pure a
          Read a    -> a <$> Prelude.getLine

interpretWrite :: Free ConsoleF a -> Writer [String] a
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
