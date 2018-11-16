---
title: Typeclasses - Functor
---

Let's think about things that might or might not happen.

```
data Perhaps a = Yeah a | Nah
```

Perhaps is a datatype that we can also use as a container for other data (by holding it inside a nice `Yeah`), or to show that we have no data with `Nah`.

Let's put things in it.

```haskell
john :: Perhaps String
john = Yeah "John"
```

Great job. John is having a nice time there. How might we express an absence of John?

```haskell
nope :: Perhaps String
nope = Nah
```

OK. No Johns here.

So what if we have a function that receives something wrapped in a `Perhaps` and wants to do stuff with it.

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```
