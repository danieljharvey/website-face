module Stringtime where

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

