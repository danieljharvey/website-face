---
title: Strange Semigroups
---

Previously we discussed how Semigroups take concepts you do understand, like combining strings and lists, and abstract them till you don't understand again. Thanks Semigroups. Themigroups.

To further muddy the water, here are some more examples of Semigroups that should make things even less clear.

They can also apply to the behaviour of numbers. Integers, for instance, can form several Monoids. One is addition:

```haskell
newtype MySum a = MySum {
    getMySum :: a
}

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)
```

Here, we have created a `newtype`, which is a wrapper around a value giving a new type, that allows us to treat it differently. We create a value with the `MySum` constructor, or we run `getMySum` on the value to get it out again. How might we use this?

```haskell
ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2
-- ten == 10
```

Seems a slightly complicated way to do things, but sure, seems fine.

Another example is multiplication:

```haskell
newtype MyProduct a = MyProduct {
    getMyProduct :: a
}

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct a <> MyProduct b = MyProduct (a * b)

sixtySix :: Int
sixtySix = getMyProduct $ MyProduct 11 <> MyProduct 2 <> MyProduct 3
-- sixtySix = 66
```
