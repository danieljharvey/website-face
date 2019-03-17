---
title: Datatypes - Either
tags: haskell, datatype, either
---

It's one thing to understand typeclasses individually, but another to see them in context. This is the second in a series looking at some common datatypes and see how their instances of the main typeclasses act. We started with one of the simplest, `Maybe`, and now we move onto it's slightly more powerful cousin `Either`.

### One thing or the other thing

Here is the data definition for `Either`. We have used `deriving` to auto-generate instances of the `Eq`, `Ord` and `Show` typeclasses as we don't need anything special going on with them.

```haskell
data Either a b
  = Left a
  | Right b
  deriving (Eq, Ord, Show)
```

Whilst `Maybe` only had one type parameter, `a`, `Either` has both `a` and `b`. If we use the `Left` constructor, it holds an `a`, or the `Right` constructor holds a `b`.

Here, we are returning either a `Left String` with some harsh words, or a `Right Int` with a nice answer to an easy sum.

```haskell
incrementOrTellMeOff :: Int -> Either String Int
incrementOrTellMeOff i
  = if i == 6
    then (Left "I hate the number six, why have you done this?")
    else (Right (i + 1))

incrementOrTellMeOff 1 -- Right 2
incrementOrTellMeOff 6 -- Left "I hate the number six, why have you done this?"
```

### Functor

Next we'll define a `functor` instance for `Either`. The important intuition here is that the function `f` that is provided will only be run on a value inside `Right`. Anything held in a `Left` will be returned untouched.

```haskell
instance Functor (Either a) where
  fmap f (Right b) = Right (f b) -- run the function over Right
  fmap _ (Left a)  = Left a      -- ignore the function for Left

fmap (+1) (Right 1) -- Right 2
fmap (+1) (Left 1)  -- Left 1
```

The `Functor` typeclass is only allowed to affect one type parameter, so anything held in a `Left` will be returned untouched, so if we want to run functions over that, we'll need...

### Bifunctor

This instance allows us to map over the left, right or indeed both sides of a datatype. We define it with the `bimap` function which takes a function for each side.

```haskell
instance Bifunctor Either where
  bimap f _ (Left a)  = Left (f a)
  bimap _ g (Right b) = Right (g b)

bimap (+1) (+100) (Left 1)   -- Left 2
bimap (+1000) (+1) (Right 1) -- Right 2
first (+1) (Right 1)         -- Right 1
first (+1) (Left 1)          -- Left 2
second (+1) (Left 1)         -- Left 1
```

Just defining `bimap` also means we get the `first` function which just maps over the `Left` side, and the `second` function which just maps over the `Right` (so it's basically `fmap` with another name).

### Applicative

The `applicative` instance for `Either` has two functions, `pure` and `<*>` (also called `ap`). We use `pure` to define a default instance of the datatype, so we just take the value and wrap it in `Right`. The `<*>` function is used to apply a function inside a `Right` to a value wrapped in another `Right`. However, if we hit a `Left` we return the first one immediately and stop computing.

```haskell
instance Applicative (Either a) where
  pure a = Right a
  (Right f) <*> (Right a) = Right (f a)
  (Right f) <*> (Left a)  = Left a
  (Left a)  <*> \_        = Left a

Right (+1) <*> Right 1 -- Right 2
Right (+1) <*> Left 1  -- Left 1
Left 10    <*> Right 1 -- Left 10
Left 2     <*> Left 3  -- Left 2
```

Sometimes, this behaviour is not what we want as we might want to collect together all of the `Left` values rather than just the first one found. For this there is a variation on `Either` called `Validation`, which we'll look at in future.

### Monad

The `monad` instance for `Either` provides us with `>>=` (or `bind`). The behaviour is much like the `Applicative` above or indeed the `Monad` instance for `Maybe` - as soon as an error is found (ie, a `Left`) we return that error value and shortcircuit the computation, as such.

```haskell
instance Monad (Either a) where
  Right a >>= k = k a
  Left e >>= \_ = Left e

Right 10 >>= \a -> Right (a + 1))      -- Right 11
Left "oh no" >>= \a -> Right (a + 1))  -- Left "oh no"
```

### Semigroup

This `semigroup` instance for `Either` differs from the standard one in `Data.Either` as, like our `Semigroup` instance for `Maybe` it has a constraint allowing us to combine `Semigroup` values inside. I like this better, having nested `Semigroup` values all magically combining is very enjoyable to my mind.

```haskell
instance (Semigroup a) => Semigroup (Either e a) where
  (Right a) <> (Right b) = Right (a <> b)
  (Left a)  <> (Left b)  = Left a
  (Left a)  <> b         = b
  a         <> (Left b)  = a

Right [1,2,3] <> Right [4,5,6] -- Right [1,2,3,4,5,6]
Right [1,2,3] <> Left 1        -- Right [1,2,3]
Left 1 <> Right [4,5,6]        -- Right [4,5,6]
Left 1 <> Left 2               -- Left 1
```

### Monoid

Sadly, because `Either` has two type parameters rather than `Maybe`'s one, we can't have a `Monoid` instance as we won't know which value to put in `Left` to represent nothingness. Oh well.

### Foldable

If we'd like to extract our value out of this `Either` context at some point we can use `foldable`. Note that we still provide a default `a` in the `Left` version of the function - the `Left` values might be a different type to the `Right` ones so there's no guarantee they'll be helpful in our fold, hence we still provide a default value.

```haskell
instance Foldable (Either e) where
  foldr \_ a (Left e) = a
  foldr f a (Right b) = f b a

foldr (+) 1 (Left 10)  -- 1
foldr (+) 1 (Right 10) -- 11
```

### Alternative

For the same reasons that defining `mempty` is impossible for `Monoid`, we can't define `Alternative` for `Either` because it's just too messy. I'm sorry.

### MonadPlus

As I'm sure we all remember, `MonadPlus` is just `Alternative` with a more dynamic sounding name, so we don't get this one either.

### Traversable

The `traversable` instance for `Either` is very similar to `Maybe`. If `traverse` is run on a `Left` it wraps said `Left` and it's value inside whichever `Applicative` it is used with (the `pure` function coming from the other type rather than from `Either`). If we `traverse` a `Right` value then the provided function `f` is run on the `Right` values the same way in which they worked on `Just`. Note if we have several `Left` values, the shortcircuiting behaviour means we only get the first one back, shown here by using the `sequence` function (which is just `traverse id`, fact fans).

```haskell
instance Traversable (Either e) where
  traverse \_ (Left e) = pure (Left e)
  traverse f (Right a) = fmap Right (f a)

sequence [Right 1, Right 2] -- Right [1,2]
sequence [Left 1, Right 1]  -- Left 1
```

If you are ever trying to turn a `List` of `Maybe` values into a `Maybe List`, or indeed turn any pair of `Applicatives` inside out, then `sequence` is probably what you are looking for. It's magic, honestly.

### MonadFail

It turns out `MonadFail` is quite fussy about things, and so because the `e` in `Either e a` can be a `String`, but might not be, we can't define it. What a pain in the arse, all told.

### Right, right, right, right.

Anyway, that's some things. Tldr; `Either` isn't too dissimilar to `Maybe` when everything is going well, and a bit different when errors start happening. Also, `Either` has way less instances than I thought, which was somewhat a relief I have to admit. I can't decide whether to do `List` or something jazzier like `Reader` next, but it'll be something like that. Please note these aren't the same definitions as you'll find in the Haskell Prelude, as I have tried to write them with an emphasis on clarity/simplicity (or indeed go completely off-piste with `Semigroup`.) By all means check out the originals on [Hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Either.html#Either).

If having skim-read this post you find yourself with strong feelings about it (positive or otherwise) I'd appreciate you shouting them at my face via the [usual channels](/contact.html). It's lonely down here in the soupy bottom of this old council bin.

That's all.

Further reading:

[Data.Either](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html)
