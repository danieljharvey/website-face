---
title: Typeclasses - Monoid
tags: haskell, typeclasses
---

In an exciting previous episode, we talked about the [Semigroup](./posts/2019-01-03-typeclasses-semigroup.markdown) type class that is used for smashing two things together.

Today we're going to talk about the `Monoid` typeclass, which is a way of generalising a collection of things that can be combined together. Seems very similar, but the important difference between combining two things, and combining any number of things, is that that number of things might be zero things.

Therefore as well as the `append` function `<>` we will need a way of expressing what no items looks like. Seems weird? Yeah, it kind of is. Some examples may help.

### Combining Lists

We need to combine a `List` of other `List` values, and we're all functional programmers, so let's go ahead and make a function for doing that.

```haskell
combineList :: [[a]] -> [a]
combineList (a:as) = a ++ (combineList as)
```

If we use it on an actual value, then great job.

```haskell
great :: [Int]
great = combineList [[1,2,3],[4,5,6]]
-- great == [1,2,3,4,5,6]
```

But what about if we use it on an empty list?

```haskell
error :: [Int]
error = combineList []
```

Ahh shit! We get this:

```haskell
uncaught exception: PatternMatchFail
```

That's not great. Looks like we're going to need an empty value to use for empty lists.

```haskell
combineList1 :: [[a]] -> [a]
combineList1 [] = []
combineList1 (a:as) = a ++ (combineList1 as)
```

```Haskell
ok :: [Int]
ok = combineList1 []
-- ok == []
```

So here the empty value is `[]` - and having this value is what means we have a `Monoid` as well as a `Semigroup`.

### Definition

What does `ghci` say about `Monoid`?

```bash
Prelude> :i Monoid
```

```haskell
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty #-}
```

OK. So firstly, whatever we want to make a `Monoid` must also be a `Semigroup`.

(Incidentally, the terminology of the relationship between these two is that `Semigroup` is a `superclass` of `Monoid`, meaning anything that is a `Monoid` is also a `Semigroup`. Conversely `Monoid` is a `subclass` of `Semigroup`. You'll notice many of these relationships amongst Haskell classes, like between `Eq` and `Ord`, and between `Functor` and `Applicative`. More words! More confusion! Great!)

Secondly, the only function we need to define (because of `MINIMAL`) is `mempty` - which defines our empty element (we also need `mappend`, but this is the same as `<>` and is supplied by our `Semigroup` unless we have a burning desire to write another one).

Therefore, if we take a `Semigroup` and plop an `mempty` function on it too we can have a `Monoid`. Clear as mud!

### More about mempty

The important test for this `mempty` value is that when we `mappend` it onto our `Monoid` it does nothing whatsoever.

Here's an empty element for `List`...

```haskell
emptyList :: [Int]
emptyList = []
```

...and here it combined with another `List` and achieving absolutely nothing.

```haskell
addNothing :: [Int]
addNothing = [1,2,3] ++ emptyList
-- addNothing == [1,2,3]
```

### Defining a List Monoid

So we need to make a `List Monoid` then?
1. An operation called `mappend` for combining two values - here `++` will do the trick.
2. An empty value called `mempty` - here we would use `[]`.

Here goes:

```Haskell
instance Semigroup [a] where
  a <> b = a ++ b

instance Monoid [a] where
  mempty = []
```

Great stuff!

### We get mconcat for free

Once we've defined that, we get `mconcat` (or rather our nice `combineList1`) for free! All it does is a fold, starting with `mempty` and then applying `mappend` to each element in the list. Great!

### Combining Numbers

The empty element is interesting on our number `Semigroup` instances from before too.

Addition works like this, with an empty element of `0`.

```Haskell
newtype MySum a = MySum {
    getMySum :: a
}

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

instance (Num a) => Monoid (MySum a) where
    mempty = MySum 0

ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2
-- ten == 10
```

Why zero? Because adding 0 to a number does nothing!

Multiplication is not the same though.

```haskell
newtype MyProduct a = MyProduct {
    getMyProduct :: a
}

instance (Num a) => Semigroup (MyProduct a) where
    MyProduct a <> MyProduct b = MyProduct (a * b)

instance (Num a) => Monoid (MyProduct a) where
    mempty = MyProduct 1

sixtySix :: Int
sixtySix = getMyProduct $ MyProduct 11 <> MyProduct 2 <> MyProduct 3
-- sixtySix == 66
```

Here the `mempty` value must be 1, because multiplying anything by `1` changes nothing.

### So What Does It All Mean?

This all seems a lot of work to get a free `mconcat` function, but the `Monoid` typeclass really comes into it's own when used with stuff like [Foldable](./posts/2018-12-03-typeclasses-foldable.markdown). Just by adding this empty value we get a hell of a lot more "for free", as such.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Data.Monoid on Hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html)

[5 minutes to Monoids](https://medium.com/@sjsyrek/five-minutes-to-monoid-fe6f364d0bba)

[Monoids tour on School of Haskell](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour)
