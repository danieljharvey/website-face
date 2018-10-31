---
title: Learning Lens
---

Let's learn the lens!

Immutability is great, but it does mean that updating a value that lives deep within a big data structure can become an utter pain in the arse.

```haskell
data AppData = AppData {
    title :: String
  , info :: {
      firstName :: String
    , surName :: String
    , age :: Int
  }
}
```

How would we get the age from this data structure?

```javascript
const age = appData.info.age; // easy!
```

It's not too bad in Haskell either, as it auto creates selector functions for records like thus:
```haskell
getAge :: AppData -> Int
getApp appData = age (info appData)
```

(This could also be written as the less obvious `age $ info $ appData` but it's no so immediately intuitive unless you are familiar with the concept of infix precedence.)

That all seems well and fine, but how would we increase the age in this record? In mutable JS-land, this would be easy, something like:

```javascript
appData.info.age = appData.info.age + 1; // great!
```

Ignoring for a moment that we have ruined the original object and probably confused anything that depends on it, this is OK, and nicely concise. The immutable way is a bit wordier though...

```javascript
const newAppData = {
    ...appData,
    info: {
        ...appData.info,
        age: appData.info.age + 1
    }
}
```

And this will only get worse as the levels get deeper. Haskell has a similar problem with deep updates in records, here's the equivalent code:

```haskell
myAppData :: AppData
myAppData = 
    AppData { title = "Example"
            , info = { firstName = "Edward"
                     , surName = "Horse"
                     , age = 100
                     }
            }

incrementAge :: AppData -> AppData
incrementAge appData = appData { info = info appData { age = (age $ info $ appData) + 1 } }
```

Urgh. What's the solution then? Lens!

 
