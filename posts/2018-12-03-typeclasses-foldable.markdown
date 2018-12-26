---
title: Typeclasses - Foldable
---

Let's think about sets of things that we want to make into one thing.

A classic example might be a list of numbers that we want to add up.

In Javascript we might do something like this:

```javascript
const added = [1, 2, 3, 4].reduce((total, item) => {
  return total + item;
}, 0);
// added == 10
```

Or perhaps we could get the maximum of the same list.

```javascript
const maxNo = [1, 2, 3, 4].reduce((highest, item) => {
  return highest > item ? highest : item;
}, 0);
// maxNo == 4
```

So in Haskell we have the very similar `foldr` with the following signature:

```haskell
Prelude> :i Foldable
```

```haskell
class Foldable (t :: * -> *) where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  {-# MINIMAL foldMap | foldr #-}
```

(there is actually loads more but these are the key ones)

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

Here are the above JS functions using `foldr`.

```haskell
added :: Int
added = foldr (\a b -> a + b) 0 [1,2,3,4]
-- added = 10
```

```haskell
maxNo :: Int
maxNo = foldr (\a b -> if a > b then a else b) 0 [1,2,3,4]
-- maxNo = 4
```

Not hugely different from the Javascript equivalent really. If you squint you can see the combining function, the initial value, and the data itself.

`foldMap` works a little differently. Instead of taking a custom combining function and using that to combine the items together, it takes a `a -> m` function (where the `m` in question is any `Monoid` instance). It uses this to turn each item into a `Monoid`, and then uses the `<>` and `mempty` functions for that Monoid to combine the items.

Here's a `newtype` I made earlier: `MySum`. It's `Monoid` instance adds numbers together when combined.

```haskell
newtype MySum a = MySum { getMySum :: a }

-- Semigroup instance
instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

-- Monoid instance
instance (Num a) => Monoid (MySum a) where
    mempty = MySum 0
```

Now we can use `foldMap` with the `MySum` constructor to add up a list of numbers.

```haskell
addTwo :: MySum Int
addTwo = foldMap MySum [1,2,3,4]
-- addTwo = MySum 10
```

Great stuff! Now our answer is still wrapped up in a `MySum`, but it's easy enough to take it out.

```haskell
addTwoUnwrapped :: MySum Int
addTwoUnwrapped = getMySum $ foldMap MySum [1,2,3,4]
-- addTwoUnwrapped = 10
```

Excellent!

This seems laborious, but actually `MySum` isn't my invention, I've just stolen a thing called `Sum` that comes with the Haskell Prelude. Therefore we can just do `getSum $ foldMap Sum [1,2,3,4,5,6]` to Monoidally combine the list items.

It also provides a similar invention for multiplying numbers called `Product` that works like this:

```haskell
twentyFour :: Int
twentyFour = getProduct $ foldMap Product [1,2,3,4]
-- twentyFour == 24
```

Folding can also capture logic, here we are using `foldMap` to check all of a list is true.

```haskell
newtype MyAll = MyAll { getMyAll :: Bool }

instance Semigroup MyAll where
    MyAll a <> MyAll b = MyAll (a && b)

instance Monoid MyAll where
    mempty = MyAll True

allOfThem :: Bool
allOfThem = getMyAll $ foldMap MyAll [True,True,True]
-- allOfThem == True

notAll :: Bool
notAll = getMyAll $ foldMap MyAll [False, True, True]
-- notAll == False
```

(I have also stolen `MyAll`, it is usually called just `All`. You can see the pattern here.)

We could also very easily make a `MyAny` type which uses or (ie, `||`) which we could use to return a `True` whenever a single one of a collection of `Bool`s happens to be `True`. You might want to have a think about what the `mempty` value would be for that to work though. That's up to you.

Anyhow. I'm bored of typing now so I guess this is it for this one.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Data.Foldable](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Foldable.html)
