---
title: Why The Hell Should I Care About Lens? (Part 2)
tags: haskell, optics
---

So last time we looked at [lens](/posts/2018-10-30-optics-pt-1-lens.html) and saw had to jump into record-shaped structures and change things around like big hacker professionals. However we didn't try to change anything with a `sum type` in it, like `Either` or `Maybe` or something. What can we use for that? Only a bloody `Prism`, apparently.

Here's an example sum type that can either contain a dog's name or it's age, and is in no way utterly contrived.

```haskell
data DogFact = DogName String | DogAge Int
```

Here are some examples of it is in use. Here it is valiantly holding a dog's name, a `String`, with the `DogName` constructor.

```haskell
spruceBruce :: DogFact
spruceBruce = DogName "Spruce Bruce"
```

Great!

And here, instead, is a dog's age, stored as an `Int` inside `DogAge`.

```haskell
oldDog :: DogFact
oldDog = DogAge 100
```

Excellent stuff, I'm sure you'll agree.

Now, we could start making all our getters and settings by hand again like this...

```haskell
getDogNameFromDogFact :: DogFact -> Maybe String
getDogNameFromDogFact (DogName s) = Just s
getDogNameFromDogFact _           = Nothing

getDogAgeFromDogFact :: DogFact -> Maybe Int
getDogAgeFromDogFact (DogAge s) = Just s
getDogAgeFromDogFact _          = Nothing
```

...but I'm sure it's clear it's soon going to get quite verbose and we should probably use something more clever instead.

Enter `Prism`! (the crowd goes wild, etc.)

A `Prism` is like a `Lens`, except it let's you peek into a particular part of a `sum type`. Therefore we can make one `Prism` that is interested in the name of dogs...

```haskell
dogNamePrism :: Prism' DogFact String
dogNamePrism = prism' DogName (\e -> case e of
                                DogName a -> Just a
                                _         -> Nothing)
```

...and another which only cares about the age of dogs...

```haskell
dogAgePrism :: Prism' DogFact Int
dogAgePrism = prism' DogAge (\e -> case e of
                              DogAge b -> Just b
                              _        -> Nothing)
```

OK. So how do we use them?

In `lens`, we had `view` for peeking inside, `set` for changing values, and `over` for mapping over the values inside. What has `Prism` got going on?

### preview

Unlike `lens` which has `set`, `prism` has a good pal called `preview` that it uses for checking out values. It returns the result inside a `Maybe` as the value is not guaranteed to actually be there.

```haskell
dogAge :: Maybe Int
dogAge = preview dogAgePrism (DogAge 100)
-- dogAge == Just 100
```

That seems reasonable.

What about running that on `spruceBruce` (that is built from a `DogName`)?

```haskell
notDogAge :: Maybe Int
notDogAge = preview dogAgePrism spruceBruce
-- notDogAge == Nothing
```

No dice. Zero dice. A complete lack of dice. No age, no `int`, sorry.

Maybe our `dogNamePrism` can help though...?

```haskell
dogName :: Maybe String
dogName = preview dogNamePrism spruceBruce
-- dogName == Just "Spruce Bruce"
```

Yes! Good stuff. Does he work on ages though?

```haskell
notDogName :: Maybe String
notDogName = preview dogNamePrism (DogAge 69)
-- notDogName == Nothing
```

No. Damn.

### set

In `lens` we used `set` for changing the value of something nested. How does that work here?

```haskell
youngerDog :: DogFact
youngerDog = set dogAgePrism 27 (DogAge 100)
-- youngerDog == DogAge 27
```

All seems well here, pretty similar to a regular `lens`. How about we give our dog a name instead of an age?

```haskell
noNewName :: DogFact
noNewName = set dogNamePrism "Nice Name" (DogAge 100)
-- noNewName == DogAge 100
```

What's happened here? Where is the name? We've ended up with exactly what we started with! How the hell do we give the dog a name? I must admit this confused me for quite a while, until I realised that `Prism` is doing exactly as it should - letting us get at the values inside a sum type without changing it's structure.

Therefore we can change one `DogName` for another...

```haskell
newDogName :: dogFact
newDogName = set dogNamePrism "Excellent Bruce" (DogName "Steve")
-- newDogName == DogName "Excellent Bruce"
```

...but not change the name of `DogAge`.

```haskell
noNewDogName :: dogFact
noNewDogName = set dogNamePrism "Good Old Nigel" (DogAge 400)
-- noNewDogName == DogAge 400
```

### over

As well as inheriting `set` from `lens` the `prism` also has `over`, that lets us map a function over nested value.

```haskell
stillQuiteOldDog :: DogFact
stillQuiteOldDog = over dogAgePrism (-1) oldDog
-- stillQuiteOldDog == DogAge 99
```

When you think about how mapping a function doesn't change the structure around the value (hello, `functor`) then the behaviour of `set` that initially confused me so much seems a lot more reasonable. The `over` function just lets us muddle around with whatever is inside our chosen sum type, should it be there to muddle around with. As such. Clear as mud, right?

### Composition

Ok. So like all these examples, the one above is quite simple so that you can see what's going on, but like all abstractions, it can leave us thinking "thanks pal, but this all seems a little bit much like hard work, surely". But what makes it all worthwhile is when we combine a `Prism` with other optics to make a giant super-optic. Remember our example from part one?

```haskell
appData :: AppConfig
appData = AppConfig {
    count = Right 100
  , title = "Hello"
  , dbConfig = DbConfig {
      ipAddress = "127.0.0.1"
    , thePort = 8080
  }
}
```

What if we wanted to change what's inside `count`? That would be nice, wouldn't it.

First, let's make a `prism` for `count`.

```haskell
countIntPrism :: Prism' (Either Error Int) Int
countIntPrism = prism' Right (\e -> case e of
                               Right b -> Just b
                               _       -> Nothing)
```

...and a `lens` for getting a `count` out of the main `appConfig`...

```haskell
countLens :: Lens' AppConfig (Either String Int)
countLens = lens count (\app newVal -> app { count = newVal } )
```

...and then we can compose them together to make a new thing.

```haskell
fullCountInt :: Traversal' AppConfig Int
fullCountInt = countLens . countIntPrism
```

Let's use it to grab that `count` value...

```haskell
initialCount :: Maybe Int
initialCount = preview fullCountInt appData
-- initialCount == Just 100
```

Great job!

And we can use it to change things too! With `set`!

```haskell
changedCount :: appConfig
changedCount = set fullCountInt 1000 appData
-- changedCount = AppConfig
--  { value = Right 1000
--  , title = "Hello!!!"
--  , dbConfig = DbConfig
--    { ipAddress = "127.0.0.1"
--    , thePort = 8080
--    }
--  }
```

And `over`!

```haskell
incrementedCount :: appConfig
incrementedCount = set fullCountInt (+1) appData
-- incrementedCount = AppConfig
--  { value = Right 101
--  , title = "Hello!!!"
--  , dbConfig = DbConfig
--    { ipAddress = "127.0.0.1"
--    , thePort = 8080
--    }
--  }
```

All our friends are here!

You might notice the new type `Traversal` that has been produced by combining a `Lens` and a `Prism`. That's an interesting thing in itself, but we'll come to that another time.

That's quite enough.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Lens by example - Prisms](https://lens-by-example.chrispenner.ca/articles/prisms/overview)
