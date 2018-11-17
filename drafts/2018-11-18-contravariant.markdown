---
title: Contravariant, or Backwards Functors
---

```haskell
class Contravariant (f :: * -> *) where
  contramap :: (a -> b) -> f b -> f a
  (>$) :: b -> f b -> f a
  {-# MINIMAL contramap #-}
```
