---
title: Datatypes - Maybe
tags: haskell, datatype, maybe
---

It's one thing to understand typeclasses individually, but another to see them in context. This is the first in a series where we'll look at some common datatypes and see how their instances of the main typeclasses act. We're starting with one of the simplest, `Maybe`, and I hope this will help you get a feel for the way it acts. The plan is to move onto `Either`, `List` and then `Reader`, `Writer` and `State`.

### It Really Meant Nothing, Frank

Let's start with a definition. We have used `deriving` to auto-generate instances of the `Eq`, `Ord` and `Show` typeclasses as we don't need anything special going on with them.

```haskell
data Maybe a = Just a | Nothing
  deriving (Eq, Ord, Show)
```

As is hopefully apparently, `Maybe` can either be a `Just` with an `a` wrapped inside, or `Nothing` which holds no value.

```haskell
noNinePlease :: Int -> Maybe Int
noNinePlease i
  = if i == 9
    then Nothing
    else Just i

noNinePlease 8 -- Just 8
noNinePlease 9 -- Nothing
```

### Functor

Next we'll define a `functor` instance for `Maybe`. Essentially, if there is a value inside, let's run the provided function over it, and if not, return the same `Nothing`.

```haskell
instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing

fmap (+1) (Just 1) -- Just 2
fmap (+1) Nothing  -- Nothing
```

### Applicative

The `applicative` instance for `Maybe` has two functions, `pure` and `<*>` (also called `ap`). We use `pure` to define a default instance of the datatype, so we just take the value and wrap it in `Just`. The `<*>` function is used to apply a function inside a `Just` to a value wrapped in another `Just`. Therefore, if either of those are a `Nothing`, that's not going to work, so we return `Nothing`.

```haskell
instance Applicative Maybe where
  pure a = Just a
  (Just f) <*> (Just a) = Just (f a)
  _        <*> _        = Nothing

pure 1                -- Just 1
Just (+1) <*> Just 1  -- Just 2
Just (+1) <*> Nothing -- Nothing
```

### Monad

The `monad` instance for `Maybe` has only one additional function, `>>=` (or `bind`). The most important thing in the `Maybe` case is that if we start with a `Nothing`, then we don't bother doing anything, allowing the computation to be shortcircuited, as such.

```haskell
instance Monad Maybe where
  (Just a) >>= k   = k a
  Nothing  >>= _   = Nothing

Just 1 >>= (\a -> Just (a + 1))  -- Just 2
Nothing >>= (\a -> Just (a + 1)) -- Nothing
```

### Semigroup

The `semigroup` instance for `Maybe` is used to combine multiple `Maybe` values together. An important thing to note is the constraint `Semigroup a` - this means that for two `Maybe` values to be combined, the values inside must also have a `semigroup` instance, allowing them to be combined as well.

```haskell
instance (Semigroup a) => Semigroup (Maybe a) where
  (Just a) <> (Just b) = Just (a <> b)
  Nothing  <> a        = a
  a        <> Nothing  = a

Just [1,2,3] <> Just [4,5,6] -- Just [1,2,3,4,5,6]
Nothing <> Just [1,2,3]      -- Just [1,2,3]
Just [1,2,3] <> Nothing      -- Just [1,2,3]
Nothing <> Nothing           -- Nothing
```

### Monoid

Notice that when a `Just` is combined with a `Nothing`, we still get a `Just` value. This is because `Nothing` is our `empty` element, meaning that when it is combined to any value it does not change it. Defining this upgrades our `semigroup` instance into an exciting `Monoid`.

```haskell
instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing

mempty -- Nothing
```

### Foldable

Now carrying all these wrapped values around is great, but at some point we may want to extract values from these `Maybe` values, so we use `foldable`. Note the `a` in the `Nothing` version of the function - this makes the user of the typeclass provide a default value so that we don't end up without a value for `Nothing`.

```haskell
instance Foldable Maybe where
  foldr _ a Nothing  = a
  foldr f a (Just b) = f b a

foldr (+) 1 Nothing   -- 1
foldr (+) 1 (Just 10) -- 11
```

### Alternative

A good intuition for the `Alternative` typeclass is that it's a like the `or` operator `||`. Therefore it can be used to return the first out of a list of values that is wrapped in `Just`.

```haskell
instance Alternative Maybe where
  empty                 = Nothing
  (Just a) <|> _        = Just a
  Nothing  <|> (Just b) = Just b
  Nothing  <|> Nothing  = Nothing

Nothing <|> Nothing -- Nothing
Just 1 <|> Just 2   -- Just 1
Nothing <|> Just 2  -- Just 2
```

### MonadPlus

Since we're on a roll with defining typeclasses, let's plop in a quick instance of `MonadPlus`, which is basically `Alternative` with a different name, with `mzero` replacing `empty` and `mplus` replacing `<|>`.

```haskell
instance MonadPlus Maybe where
  mzero = Nothing

  mplus (Just a) _ = Just a
  mplus (Nothing) (Just b) = Just b
  mplus _ _ = Nothing


Nothing `mplus` Nothing -- Nothing
Just 1 `mplus` Nothing  -- Just 1
Nothing `mplus` Just 2  -- Just 2
mzero                   -- Nothing
```

### Traversable

The `traversable` instance for `Maybe` isn't too unusual, if `traverse` is run on a `Nothing` it wraps a `Nothing` inside whichever `Applicative` it is used with (the `pure` function coming from the other type rather than from `Maybe`). If we `traverse` a `Just` value then the provided function `f` is run on the value inside `Just` which wraps the `a` in an `applicative` functor, and we then use `fmap` to make the value inside a `Just`.

```haskell
instance Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just a) = fmap Just (f a)

traverse (\a -> [a,a]) Nothing   -- [Nothing]
traverse (\a -> [a,a]) (Just 10) -- [Just 10, Just 10]
```

### MonadFail

The `MonadFail` typeclass hasn't come up before, but it's a very generic way of allowing all computations to fail in a similar way. The `fail` function has the type signature `String -> m a`. However because we cannot carry around any values inside `Nothing` we simply discard the `String` and return `Nothing`. The usefulness of this typeclass will become much more apparent with `Either` and monad transformer stacks. (what? - we'll come to it...)

```haskell
instance MonadFail Maybe where
  fail _ = Nothing

Just "yes" >>= fail -- Nothing
```

### Just "a great job"

Anyway. This is quite a laborious post but I hope it is somewhat helpful. I intend to do similar ones for `Either` and `List` next. Please note these aren't the same definitions as you'll find in the Haskell Prelude, as I have tried to write them with an emphasis on clarity/simplicity. If you are a purist, or just bloody hate clarity, by all means check out the originals on [Hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-854).

If having skim-read this post you find yourself with strong feelings about it (positive or otherwise) I'd appreciate you shouting them at my face via the [usual channels](/contact.html). It's lonely out here in the bleak abyss of South East London.

That's all.

Further reading:

[Data.Maybe](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html)
