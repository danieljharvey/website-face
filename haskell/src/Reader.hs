module Reader where

newtype Reader r a
  = Reader { runReader :: r -> a }

basic :: Reader String String
basic = Reader (\r -> "Hello, " <> r)

runningIt :: String
runningIt = runReader basic "Dog"
-- "Hello Dog"


instance Functor (Reader r) where
  fmap f (Reader a) = Reader (f <$> a)

mapped :: String
mapped = runReader (fmap (++ "!!!!") basic) "Dog"
