---
title: Typeclasses - Semigroup
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

Sure! Seems great. So what's `Semigroup` got to do with this? `Semigroup` is the generalisation of combining things together. How is it defined?

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

It doesn't just involve putting sets of things together. They can also apply to the behaviour of numbers. Integers, for instance, can form several Monoids. One is addition. Let's build a newtype so that we can make a `Semigroup` instance.

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

This is obviously a fairly over-complicated way to add numbers together, but bear with me here.

Or we do the same for multiplication.

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
