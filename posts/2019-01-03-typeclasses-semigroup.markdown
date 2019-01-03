---
title: Typeclasses - Semigroup
tags: haskell, typeclasses
---

Let's think about things that can be put together.

Here's two lists, combined into one big list.

```haskell
list :: [Int]
list = [1,2,3] ++ [4,5,6]
-- list == [1,2,3,4,5,6]
```

Great!

What about two strings combined into one large excellent string?

```haskell
string :: String
string = "Great" ++ " Stuff"
-- string == "Great Stuff"
```

Sure! Seems great. `String` is actually a `List` of `Char` so really that's just the same thing happening twice there. So what's `Semigroup` got to do with this? `Semigroup` is the generalisation of combining things together. How is it defined?

### Definition

```haskell
Prelude> :i Semigroup
```

...gets us...

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
  {-# MINIMAL (<>) #-}
```

So by the looks we can just define it with the `<>` function.

Therefore we could write our string concat function as `"Great" <> "Job"` or combine lists like `[1,2,3] <> [4,5,6]`.

It doesn't just involve putting sets of things together.

### Maybe Semigroup

Let's define our own datatype `MyMaybe` which you may notice is very similar to `Maybe`. The `Semigroup` instance can be used to combine two of them.

```haskell
data MyMaybe a = Yeah a | Nope
```

What's interesting about this one is that by insisting that the value inside is also a `Semigroup`, we can do some exciting multi-level combining.

```haskell
instance (Semigroup a) => Semigroup (MyMaybe a) where
  (Yeah a) <> (Yeah b) = Yeah (a <> b)
  a        <> Nope     = a
  Nope     <> b        = b
```

Let's see it at work.

```haskell
nah :: MyMaybe String
nah = Nope <> Nope
-- nah == Nope
```

Sure.

```haskell
first :: MyMaybe String
first = Yeah "Totally" <> Nope
-- first == Yeah "Totally"
```

Plausible.

```haskell
second :: MyMaybe String
second = Nope <> Yeah "Great"
-- second == Yeah "Great"
```

OK.

And the more interesting one...

```haskell
both :: MyMaybe String
both = Yeah "Totally" <> Yeah "Great"
-- both = Yeah "TotallyGreat"
```

What happened here? We combined two `MyMaybe` values AND the `String` values inside them as well, without really putting much effort in whatsoever. Great job, `Semigroup`.

### Sum Semigroup

We can also use this pattern to describe combining numbers. Integers, for instance, can form several Monoids. One is addition. Let's build a [newtype](./posts/2018-12-19-new-types.markdown) so that we can make a `Semigroup` instance.

```haskell
newtype MySum a = MySum {
    getMySum :: a
}
```

Here is a `Semigroup` instance, it uses pattern matching to take the original values out, adds them together, them wraps them in another `MySum` instance.

```haskell
instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)
```

We can create several `MySum` instances, and then combine them together with `<>`.

```haskell
ten :: MySum Int
ten = MySum 1 <> MySum 7 <> MySum 2
-- ten == MySum 10
```

Or we could use the `getMySum` function in the newtype to unwrap it.

```haskell
anotherTen :: Int
anotherTen = getMySum $ MySum 1 <> MySum 7 <> MySum 2
-- anotherTen == 10
```

(This is obviously quite an overwrought way to add 3 numbers together)

### Product Semigroup

We could also combine numbers by multiplying them together.

```haskell
newtype MyProd a = MyProd {
    getMyProd :: a
}
```

```haskell
instance (Num a) => Semigroup (MyProd a) where
    MyProd a <> MyProd b = MyProd (a * b)
```

```haskell
sixtySix :: Prod Int
sixtySix = MyProd 11 <> MyProd 2 <> MyProd 3
-- sixtySix = Prod 66
```

```haskell
anotherSixtySix :: Int
anotherSixtySix = getMyProd $ MyProd 11 <> MyProd 2 <> MyProd 3
-- anotherSixtySix = 66
```

### So, yes.

Hopefully you get the idea of what's going on here. A `Semigroup` is a nice way of describing things we can smash together, and it becomes even better when we extend it into `Monoid`, that gets us a load of `Foldable` stuff for free. We'll come to that soon...

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Data.Semigroup](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Semigroup.html)
