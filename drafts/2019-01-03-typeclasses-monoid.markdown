---
title: Typeclasses - Monoid
tags: haskell, typeclasses
---

Today we're going to talk about the `Monoid` typeclass, which is a way of generalising things that can be combined together. I found it one of the trickiest to get my head around because what it did was so straightforward that I was sort of annoyed that I had to learn a new word (especially one so confusingly similar to `Monad`).

We'll explain it in terms of combining `List` values, and then move onto some interesting examples.

### Combining Lists

We need to combine two `List` values, and we're all functional programmers, so let's go ahead and make a function for doing that.

```haskell
listCombine :: [Int] -> [Int] -> [Int]
listCombine a b = a ++ b
```

Classic stuff. Let's see it work.

```haskell
bothLists :: [Int]
bothLists = listCombine [1,2,3] [4,5,6]
-- bothLists == [1,2,3,4,5,6]
```

Nothing untoward going on here.

For it to be a proper classic `Monoid` it also needs another thing, an empty value. We'll know we have a good one if, when we plop it on the end of a list, does nothing.

```haskell
emptyList :: [Int]
emptyList = []
```

If we add this anywhere to a `List`, it gives us the same `List`, like this:

```haskell
addNothing :: [Int]
addNothing = listCombine [1,2,3] emptyList
-- addNothing == [1,2,3]
```

OK! Great. So how do we make these into an actual typeclass instance?

What does `ghci` say?

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

OK. A few things.

The reason for the empty element may not be immediately clear, but it makes more sense when we use them for folding with [Foldable](./posts/2018-12-03-typeclasses-foldable.markdown).

What do we need to make something a `Monoid` then?
1. An operation called `mappend` for combining two values - here our `strConcat` function.
2. An empty value called `mempty` - here we could use `stringEmpty`.



### Combining Lists



### Combining Numbers

### So What Does It All Mean?

Further reading:

[Data.Monoid on Hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html)

[5 minutes to Monoids](https://medium.com/@sjsyrek/five-minutes-to-monoid-fe6f364d0bba)

[Monoids tour on School of Haskell](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour)

### Bonus: Monoids All The Way Down

fsglslsdfgjkl
