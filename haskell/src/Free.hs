module Free where

import           Control.Monad.Free
import           Prelude

data ConsoleF a
  = PutStrLn String a
  | GetLine (String -> a)

instance Functor ConsoleF where
    fmap f (PutStrLn s a) = PutStrLn s (f a)
    fmap f (GetLine g)    = GetLine (f . g)

type Console = Free ConsoleF

fPutStrLn :: String -> Console ()
fPutStrLn s = liftF (PutStrLn s ())

fGetLine :: Console String
fGetLine = liftF (GetLine id)

-- Console in IO:
-- consoleIO :: ConsoleF () -> IO ()
consoleIO (PutStrLn s v) = do
  Prelude.putStrLn s
  pure v
consoleIO (GetLine cb) = do
  s <- Prelude.getLine
  pure (cb s)

consoleProg :: Console ()
consoleProg = do
    fPutStrLn "What the fuck?"
    a <- fGetLine
    fPutStrLn $ "Sure, I guess? " ++ a
    b <- fGetLine
    fPutStrLn "bum"

