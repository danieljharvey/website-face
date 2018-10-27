---
title: Semigroup, semipoint
---

Last time we did a terrible job of explaining a Monoid.

A Monoid has two properties.

1. a function for combining two of them
2. the concept of an empty item, that makes no difference when it is combined with any other value

A Semigroup is like a Monoid, but it only does one of those things, and that is 1.

Therefore, it only really makes sense to use it on things that are never empty.

Let's look at our made-up `NonEmpty` datatype:

```haskell
data NonEmpty a = NonEmpty a [a]
```

