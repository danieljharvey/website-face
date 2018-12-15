module Lenses where

import           Control.Lens
import           Control.Lens.Prism

type Error = String

data DbConfig = DbConfig { ipAddress :: String
                         , thePort   :: Int
                         } deriving (Show)

data AppConfig = AppConfig { count    :: Either Error Int
                           , title    :: String
                           , dbConfig :: DbConfig
                           } deriving (Show)

appData :: AppConfig
appData = AppConfig {
    count = Right 100
  , title = "Hello"
  , dbConfig = DbConfig {
      ipAddress = "127.0.0.1"
    , thePort = 8080
  }
}

getPort :: AppConfig -> Int
getPort app = thePort $ dbConfig app

incrementPort :: AppConfig -> AppConfig
incrementPort app =
    app { dbConfig = (dbConfig app) { thePort = oldPort + 1} }
        where oldPort = getPort app

setPort :: Int -> AppConfig -> AppConfig
setPort port app =
    app { dbConfig = (dbConfig app) { thePort = port } }


getCountInt :: AppConfig -> Maybe Int
getCountInt app = case count app of
                    Right i -> Just i
                    _       -> Nothing

getCountError :: AppConfig -> Maybe Error
getCountError app = case count app of
                      Left i -> Just i
                      _      -> Nothing

setCountInt :: Int -> AppConfig -> AppConfig
setCountInt int app =
    app { count = Right int }

setCountError :: String -> AppConfig -> AppConfig
setCountError err app =
    app { count = Left err }

-- As you can see, the above will soon get a bit tiresome, let's Lens it!
titleLens :: Lens' AppConfig String
titleLens = lens title (\app newTitle -> app { title = newTitle } )

dbConfigLens :: Lens' AppConfig DbConfig
dbConfigLens = lens dbConfig (\app db -> app { dbConfig = db })

portLens :: Lens' DbConfig Int
portLens = lens thePort (\db port -> db { thePort = port } )

fullPortLens :: Lens' AppConfig Int
fullPortLens = dbConfigLens . portLens

-- Let's use them!
appTitle :: String
appTitle = view titleLens appData -- = "Hello"

-- And what about those Either types, can we do anything there? Sure!

type Dog = Either String Int

dogString :: Dog
dogString = Left "Dog Name"

dogInt :: Dog
dogInt = Right 100

dogStringPrism :: Prism' Dog String
dogStringPrism = prism' Left (\e -> case e of
                            Left a -> Just a
                            _      -> Nothing)

dogIntPrism :: Prism' Dog Int
dogIntPrism = prism' Right (\e -> case e of
                            Right b -> Just b
                            _       -> Nothing)


-- and what about combining them with our previous lens?
countLens :: Lens' AppConfig (Either String Int)
countLens = lens count (\app newVal -> app { count = newVal } )

countErrorPrism :: Prism' (Either Error Int) Error
countErrorPrism = prism' Left (\e -> case e of
                            Left a -> Just a
                            _      -> Nothing)


countIntPrism :: Prism' (Either Error Int) Int
countIntPrism = prism' Right (\e -> case e of
                                      Right b -> Just b
                                      _       -> Nothing)


fullCountError :: Traversal' AppConfig Error
fullCountError = countLens . countErrorPrism

fullCountInt :: Traversal' AppConfig Int
fullCountInt = countLens . countIntPrism
