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
const max = [1, 2, 3, 4].reduce((highest, item) => {
  return highest > item ? highest : item;
}, 0);
// max == 4
```

So in Haskell we have the very similar `foldr` with the following signature:

```haskell
Prelude> :i Foldable
```

```haskell
class Foldable (t :: * -> *) where
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  Data.Foldable.toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
```

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```
