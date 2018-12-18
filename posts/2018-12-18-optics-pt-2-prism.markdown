---
title: Why The Fuck Should I Care About Lens? (Part 2)
---

So last time we looked at [lens](/posts/2018-10-30-optics-pt-1-lens.html) and saw had to jump into record-shaped structures and change things around like big hacker professionals. However we didn't try to change anything with a `Sum type` in it, like `Either` or `Maybe` or something. What can we use for that? Only a bloody `Prism`, apparently.

Here's an example sum type that can either contain a dog's name or it's age, and is in no way utterly contrived.

```haskell
data DogFact = DogName String
             | DogAge Int deriving (Show, Eq)
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

`preview` is for checking out a value. It returns the result inside a `Maybe` as it may not be there.

```haskell
dogAge :: Maybe Int
dogAge = preview dogAgePrism oldDog
-- dogAge == Just 100
```

When we preview `oldDog` from earlier (that is built from a `DogAge` constructor) then great, we get the age out.

What about running that on `spruceBruce` (that is built from a `DogName`)?

```haskell
notDogAge :: Maybe Int
notDogAge = preview dogAgePrism spruceBruce
-- notDogAge == Nothing
```

No dice. Zero dice. A complete lack of dice.

However, using our `dogNamePrism` let's us check the names instead.

```haskell
dogName :: Maybe String
dogName = preview dogNamePrism spruceBruce
-- dogName == Just "Spruce Bruce"

notDogName :: Maybe String
notDogName = preview dogNamePrism oldDog
-- notDogName == Nothing
```

All make sense?

What else can we do? How about a bit of `set` for changing values?

```haskell
-- can change value on same side
newAge :: DogFact
newAge = set dogAgePrism 27 oldDog
-- newAge == DogAge 27

-- but not change the side
noNewAge :: DogFact
noNewAge = set dogAgePrism 27 spruceBruce
-- noNewAge == DogName "Spruce Bruce"
```
