---
title: Datatypes - Maybe
tags: haskell, datatype, maybe
---

It's one thing to understand typeclasses individually, but another to see them
in context. Let's take at one of the simplest, `Maybe`, define it's instances,
and get a feel for how it acts.

Let's start with a definition. We have used `deriving` to auto-generate
instances of the `Eq`, `Ord` and `Show` typeclasses as we don't need anything
special going on with them.

```haskell
data Maybe a = Just a | Nothing
  deriving (Eq, Ord, Show)
```

As is hopefully apparently, `Maybe` can either be a `Just` with an `a` wrapped
inside, or `Nothing` which holds no value.

### Functor

Next we'll define a `functor` instance for `Maybe`. Essentially, if there is a
value inside, let's run the provided function over it, and if not, return the
same `Nothing`.

```haskell
instance Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing
```

```haskell
one :: Maybe Int
one = fmap (+1) (Just 1)
-- one == Just 2
```

```haskell
two :: Maybe Int
two = fmap (+1) Nothing
-- two == Nothing
```
 
### Applicative

The `applicative` instance for `Maybe` has two functions, `pure` and `<*>`
(also called `ap`). We use `pure` to define a default instance of the datatype,
so we just take the value and wrap it in `Just`. The `<*>` function is used to
apply a function inside a `Just` to a value wrapped in another `Just`.
Therefore, if either of those are a `Nothing`, that's not going to work, so we
return `Nothing`.

```haskell
instance Applicative Maybe where
  pure a = Just a
  (Just f) <*> (Just a) = Just (f a)
  _        <*> _        = Nothing
```

```haskell
three :: Maybe Int
three = pure 1 
-- three == Just 1
```

```haskell
four :: Maybe Int
four = Just (+1) <*> Just 1
-- four == Just 2
```

```haskell
five :: Maybe Int
five = Just (+1) <*> Nothing
-- five == Nothing
```
 
### Monad

The `monad` instance for `Maybe` has only one additional function, `>>=` (or
`bind`). The most important thing in the `Maybe` case is that if we start with
a `Nothing`, then we don't bother doing anything, allowing the computation to
be shortcircuited, as such.

```haskell
instance Monad Maybe where
  (Just a) >>= k   = k a
  Nothing  >>= _   = Nothing
```

```haskell
six :: Maybe Int
six = Just 1 >>= (\a -> Just (a + 1))
-- six == Just 2
```

```haskell
seven :: Maybe Int
seven = Nothing >>= (\a -> Just (a + 1))
-- seven == Nothing
```

### Semigroup

The `semigroup` instance for `Maybe` is used to combine multiple `Maybe` values
together. An important thing to note is the constraint `Semigroup a` - this
means that for two `Maybe` values to be combined, the values inside must also
have a `semigroup` instance, allowing them to be combined as well.

```haskell
instance (Semigroup a) => Semigroup (Maybe a) where
  (Just a) <> (Just b) = Just (a <> b)
  Nothing  <> a        = a
  a        <> Nothing  = a
```

### Monoid

Notice that when a `Just` is combined with a `Nothing`, we still get a `Just` value. This is because `Nothing` is our `empty` element, meaning that when it is combined to any value it does not change it. Defining this upgrades our `semigroup` instance into an exciting `Monoid`.

```haskell
instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing
```

### Foldable

Now carrying all these wrapped values around is great, but at some point we may
want to extract values from these `Maybe` values, so we use `foldable`. Note
the `a` in the `Nothing` version of the function - this makes the user of the
typeclass provide a default value so that we don't end up without a value for
`Nothing`.

```haskell
instance Foldable Maybe where
  foldr _ a Nothing  = a
  foldr f a (Just b) = f b a
```

### Alternative

A good intuition for the `Alternative` typeclass is that it's a like the `or` operator `||`. Therefore it can be used to return the first out of a list of values that is wrapped in `Just`.

```haskell
instance Alternative Maybe where
  empty                 = Nothing
  (Just a) <|> _        = Just a
  Nothing  <|> (Just b) = Just b
  Nothing  <|> Nothing  = Nothing
```

### MonadPlus

Since we're on a roll with defining typeclasses, let's plop in a quick instance of `MonadPlus`, which is basically `Alternative` with a different name, with `mzero` replacing `empty` and `mplus` replacing `<|>`.

```haskell
instance MonadPlus Maybe where
  mzero = Nothing

  mplus (Just a) _ = Just a
  mplus (Nothing) (Just b) = Just b
  mplus _ _ = Nothing  
```

### Traversable

The `traversable` instance for `Maybe` isn't too unusual, if `traverse` is run on a `Nothing` it wraps a `Nothing` inside whichever `Applicative` it is used with (the `pure` function coming from the other type rather than from `Maybe`). If we `traverse` a `Just` value then the provided function `f` is run on the value inside `Just` which wraps the `a` in an `applicative` functor, and we then use `fmap` to make the value inside a `Just`.

```haskell
instance Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just a) = fmap Just (f a)
```

### MonadFail

The `MonadFail` typeclass hasn't come up before, but it's a very generic way of allowing all computations to fail in a similar way. The `fail` function has the type signature `String -> m a`. However because we cannot carry around any values inside `Nothing` we simply discard the `String` and return `Nothing`.

```haskell
instance MonadFail Maybe where
  fail _ = Nothing
```


       it "Maybe Semigroup with Just" $
      Just [1,2,3] <> Just [4,5,6] `shouldBe` Just [1,2,3,4,5,6]
    it "Maybe Semigroup with Nothing" $
      Nothing <> Just [1,2,3] `shouldBe` Just [1,2,3]
    it "Maybe Semigroup with other Nothing" $
      Just [1,2,3] <> Nothing `shouldBe` Just [1,2,3]
    it "Maybe Semigroup with all Nothing" $
      Nothing <> Nothing `shouldBe` (Nothing :: Maybe [Int])
    it "Maybe Monoid" $
      mempty `shouldBe` (Nothing :: Maybe [Int])
    it "Foldable Maybe with Nothing" $
      foldr (+) 1 Nothing `shouldBe` 1
    it "Foldable Maybe with Just" $
      foldr (+) 1 (Just 10) `shouldBe` 11
    it "Alternative ending in nothing" $
      (Nothing <|> Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "Alternative first" $
      (Just 1 <|> Just 2) `shouldBe` Just 1
    it "Alternative second" $
      (Nothing <|> Just 2) `shouldBe` Just 2
    it "Traverses list Nothing" $
      traverse (\a -> [a,a]) (Nothing :: Maybe Int) `shouldBe` [Nothing]
    it "Traverses list and Just 1" $
        (traverse (\a -> [a,a]) $ Just 10) `shouldBe` [Just 10, Just 10]
    it "Monad fail returns Nothing" $
      (Just "yes" >>= fail) `shouldBe` (Nothing :: Maybe String)
    it "MonadPlus append Nothings" $
      (Nothing `mplus` Nothing) `shouldBe` (Nothing :: Maybe Int)
    it "MonadPlus append first Just" $
      (Just 1 `mplus` Nothing) `shouldBe` Just 1
    it "MonadPlus append second Just" $
      (Nothing `mplus` Just 2) `shouldBe` Just 2
    it "MonadPlus empty" $
      (mzero :: Maybe Int) `shouldBe` Nothing
    it "MonadZip mzipWith" $
      mzip (Just 1) (Just 2) `shouldBe` Just (1,2)
    it "MonadZip mzipWith fail" $
      mzip (Just 1) Nothing `shouldBe` (Nothing :: Maybe (Int, Int))
 
