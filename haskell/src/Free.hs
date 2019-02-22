module Free where

import           Control.Monad.Free
import           Prelude
import           System.Exit

data ConsoleF next
  = Write String next
  | Read (String -> next)
  | Stop

instance Functor ConsoleF where
    fmap f (Write s next) = Write s (f next)
    fmap f (Read next)    = Read (f . next)
    fmap _ Stop           = Stop

fWrite :: String -> Free ConsoleF ()
fWrite str = liftF $ Write str ()

fRead :: Free ConsoleF String
fRead = liftF $ Read id

fStop :: Free ConsoleF a
fStop = liftF Stop

consoleProg :: Free ConsoleF ()
consoleProg = do
    fWrite "What is your name?"
    a <- fRead
    fWrite $ "Sure? " ++ a
    b <- fRead
    fWrite "Great."
    fStop

interpretIO :: Free ConsoleF a -> IO a
interpretIO (Pure r) = return r
interpretIO (Free (Write s next))
  = Prelude.putStrLn s >> interpretIO next
interpretIO (Free (Read next))
  = Prelude.getLine >>= interpretIO . next
interpretIO (Free Stop)
  = exitSuccess

