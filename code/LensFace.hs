module LensFace where

import           Control.Lens
import           Control.Lens.Prism

main :: IO ()
main = print "what"

data DbConfig = DbConfig { ipAddress :: String
                         , thePort   :: Int
                         } deriving (Show)

data AppConfig = AppConfig { value    :: Either String Int
                           , dbConfig :: DbConfig
                           } deriving (Show)

getPort :: AppConfig -> Int
getPort app = thePort $ dbConfig app

setPort :: Int -> AppConfig -> AppConfig
setPort port app =
    app { dbConfig = (dbConfig app) { thePort = port } }


getValueInt :: AppConfig -> Maybe Int
getValueInt app = case value app of
                    Right i -> Just i
                    _       -> Nothing

getValueError :: AppConfig -> Maybe String
getValueError app = case value app of
                      Left i -> Just i
                      _      -> Nothing

setValueInt :: Int -> AppConfig -> AppConfig
setValueInt val app =
    app { value = Right val }

setValueError :: String -> AppConfig -> AppConfig
setValueError str app =
    app { value = Left str }

-- As you can see, the above will soon get a bit tiresome, let's Lens it!
dbConfigLens :: Lens' AppConfig DbConfig
dbConfigLens = lens dbConfig (\app db -> app { dbConfig = db })

portLens :: Lens' DbConfig Int
portLens = lens thePort (\db port -> db { thePort = port } )

fullPortLens :: Lens' AppConfig Int
fullPortLens = dbConfigLens . portLens

-- And what about those Either types, can we do anything there? Sure!

valueLens :: Lens' AppConfig (Either String Int)
valueLens = lens value (\app value -> app { value = value } )

valueErrorPrism :: Prism' (Either String Int) String
valueErrorPrism = prism' Left (\e -> case e of
                            Left a -> Just a
                            _      -> Nothing)


valueIntPrism :: Prism' (Either String Int) Int
valueIntPrism = prism' Right (\e -> case e of
                                      Right b -> Just b
                                      _       -> Nothing)


fullValueError :: Traversal' AppConfig String
fullValueError = valueLens . valueErrorPrism

fullValueInt :: Traversal' AppConfig Int
fullValueInt = valueLens . valueIntPrism
