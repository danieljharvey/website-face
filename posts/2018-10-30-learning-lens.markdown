---
title: Learning Lens
---

Let's learn the lens!

Immutability is great, but it does mean that updating a value that lives deep within a big data structure can become an utter pain in the arse. Let's define a data structure:

```haskell
data AppConfig = AppConfig { value    :: Either String Int
                           , title    :: String
                           , dbConfig :: DbConfig
                           } deriving (Show)
```

It contains `DbConfig` which looks like this:

```haskell
data DbConfig = DbConfig { ipAddress :: String
                         , thePort   :: Int
                         } deriving (Show)
```

And here is an example of the data:

```haskell
appData :: AppConfig
appData = AppConfig {
    value = Right 100
  , title = "Hello"
  , dbConfig = DbConfig {
      ipAddress = "127.0.0.1"
    , thePort = 8080
  }
}
```

How would we get data from this structure in something like javascript?

```javascript
const thePort = appData.dbConfig.thePort; // easy!
```

It's not too bad in Haskell either, as it auto creates selector functions for records like thus:

```haskell
getPort :: AppConfig -> Int
getPort app = thePort (dbConfig app)
```

(This could also be written as `thePort $ dbConfig app` but lets keep things simple.)

That all seems well and fine, but how would we change the port in this app config? In mutable JS-land, this would be easy, something like:

```javascript
appData.dbConfig.thePort = appData.dbConfig.thePort + 1;
```

Ignoring for a moment that we have ruined the original object and probably confused anything that depends on it, this is OK, and nicely concise. The immutable way is a bit wordier though...

```javascript
const newAppData = {
  ...appData,
  dbConfig: {
    ...appData.dbConfig,
    thePort: appData.dbConfig.thePort + 1
  }
};
```

And this will only get worse as the levels get deeper. Haskell has a similar problem with deep updates in records, here's the equivalent code:

```haskell
incrementPort :: AppConfig -> AppConfig
incrementPort app =
    app { dbConfig = (dbConfig app) { thePort = oldPort + 1} }
        where oldPort = getPort app
```

The worst thing about the code above is that the only really relevant part is `thePort = oldPort + 1`. Urgh. What's the solution then? Lens!

A lens captures the idea of both a `getter` and a `setter` of a piece of data and a sub-part of it. Here is one for getting `title` from an `AppConfig`.

```haskell
titleLens :: Lens' AppConfig String
titleLens = lens title (\app new -> app { title = new } )
```

We can get the title of the app using `view`:

```haskell
title = view titleLens appData

-- title = "Hello"
```

We can change the app's title using `set`:

```haskell
newApp = set titleLens "New Title" appData

-- newApp = AppConfig
--  { value = Right 100
--  , title = "New Title"
--  , dbConfig = DbConfig
--    { ipAddress = "127.0.0.1"
--    , thePort = 8080
--    }
--  }
```

Lastly, we can map over the app's title using `over`:

```haskell
newApp = over titleLens (\a -> a ++ "!!!") appData

-- newApp = AppConfig
--  { value = Right 100
--  , title = "Hello!!!"
--  , dbConfig = DbConfig
--    { ipAddress = "127.0.0.1"
--    , thePort = 8080
--    }
--  }
```

OK. All so well and good, but this does not solve our deep structure update problem. How might we change `thePort` inside `dbConfig` in the same way?

By composing Lenses!

This lens gets us the `DbConfig` inside `AppConfig`:

```haskell
dbConfigLens :: Lens' AppConfig DbConfig
dbConfigLens = lens dbConfig (\app db -> app { dbConfig = db })
```

And this one gets us `thePort` inside `DbConfig`:

```haskell
portLens :: Lens' DbConfig Int
portLens = lens thePort (\db port -> db { thePort = port } )
```

But by composing them together we get a lens that takes us from `AppConfig` all the way to `thePort`:

```haskell
fullPortLens :: Lens' AppConfig Int
fullPortLens = dbConfigLens . portLens
```

(Function composition with the `.` operator usually means that the right-hand function is run first, and then the left. With lenses, it makes more sense if we read from left to right, so if you squint it looks a little like javascript: `fullPort = dbConfig.port`. Kind of. Just nod and let's not mention this again.)

Let's use them to look at the port!

```haskell
port = view fullPortLens appData
-- port = 8080
```

To choose a new port!

```haskell
newApp = set fullPortLens 9090 appData
-- newApp = AppConfig
--  { value = Right 100
--  , title = "Hello"
--  , dbConfig = DbConfig
--    { ipAddress = "127.0.0.1"
--    , thePort = 9090
--    }
--  }
```

Or to increment the port by `1`, for some reason!

```haskell
newApp = over fullPortLens (+1) appData
-- newApp = AppConfig
--  { value = Right 100
--  , title = "Hello"
--  , dbConfig = DbConfig
--    { ipAddress = "127.0.0.1"
--    , thePort = 8081
--    }
--  }
```

Look at all the things we've done with not too much code. We are now surely mighty hackers. Next time, we'll look at another variation on `Lens` called `Prism`, and see how it let's us interact with that `Either` type we've conveniently ignored in these examples...
