---
title: Semigroup, semipoint
---

Last time we did a terrible job of explaining a Monoid.

A Monoid has two properties.

1. a function for combining two of them
2. the concept of an empty item, that makes no difference when it is combined with any other value

A Semigroup is like a Monoid, but it only does one of those things, and that is the combining part.

Therefore, it only really makes sense to use it on things that are never empty.

Let's look at our made-up `NonEmpty` datatype. We're adding `deriving (Show)` to the end so that we can print out the contents in GHCI and check our working.

```haskell
data NonEmpty a = NonEmpty a [a] deriving (Show)
```

There isn't really an `empty` element for this because even if the `[a]` part had nothing in it the `a` has to be there. What if we want to combine two `NonEmpty` lists together though?

```haskell
instance Semigroup (NonEmpty a) where
    (NonEmpty a as) <> (NonEmpty b bs) = (NonEmpty a $ as <> [b] <> bs)
```

That should work!

```haskell
first :: NonEmpty Int
first = NonEmpty 1 [2,3,4]

second :: NonEmpty Int
second = NonEmpty 5 [6,7,8]

third :: NonEmpty Int
third = first <> second

-- show third
-- "NonEmpty 1 [2,3,4,5,6,7,8]"
```

Great stuff! We have combined two items, something we could probably do anyway, using a large confusing abstraction. This is why we got into functional programming! So what's the benefit of this?

A quick look in the [Data.Semigroup.Foldable](http://hackage.haskell.org/package/semigroupoids-5.3.1/docs/Data-Semigroup-Foldable.html) package shows it has this function:

```haskell
fold1 :: Semigroup m => t m -> m
```

What does this mean? The `Semigroup m =>` part means all the `m`s inside must be instances of `Semigroup`, but the m can be anything.
