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
