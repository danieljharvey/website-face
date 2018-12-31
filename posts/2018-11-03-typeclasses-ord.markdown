---
title: Typeclasses - Ord
tags: haskell, typeclasses
---

Let's think about moods. Psychologists all agree there are only 5 real emotional states.

```haskell
data Mood = Awful | QuiteBad | OK | Good | Great
```

Which one is better? Is `Great` better than `Awful`?

```haskell
broken :: Bool
broken = Awful < Great
```

Shit!

```haskell
• No instance for (Ord Mood) arising from a use of ‘<’
• In the expression: Awful < Great
    In an equation for ‘broken’: broken = Awful < Great
```

These cannot be ordered! We need to implement the `Ord` typeclass (short for "ordering") so that we can compare these values and sort them.

What will we need to do that then?

We can find out more by firing up `ghci`, the GHC repl.

You should see a prompt with the following:

```bash
Prelude>
```

`Prelude` is telling us that we've loaded the Haskell Prelude, a standard library of functions. `Ord` is part of that, so let's get some info.

Enter `:info Ord` into the repl (and press enter). You should see something like this:

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
```

This is a list of functions that the `Ord` typeclass implements. Looks like hard work. What's interesting here?

Firstly - the `Eq a =>` constraint means that we can only define an `Ord` instance for something that has an `Eq` instance. I guess if we can't tell if two values are the same, how can we dream of putting them in some sort of order?

Secondly - the slighly cryptic `{-# MINIMAL compare | (<=) #-}` line is telling us that we can define `Ord` by either defining the `compare` or `<=` functions. Haskell can generate the other functions by using either of these ones. That's great news because hard work is hard and we don't want to do hard work.

```haskell
data SuperMood = Worse | PrettyAverage | Fine deriving (Eq)
```

We're auto-generating the `Eq` because why have a bad time. Let's make our `Ord` instance.

```haskell
instance Ord SuperMood where
    _ <= Fine = True
    Worse <= PrettyAverage = True
    _ <= _ = False
```

We're implementing it in terms of `<=` as it returns a `Bool` which is more straightforward than the `Ordering` datatype that `compare` uses).

1. `_ <= Fine = True` means that all values are equal to or less than `Fine`.
2. `Worse <= PrettyAverage = True` means that `Worse` is less than or equal to `PrettyAverage`.
3. `_ <= _ = False` means any other combination returns `False`.

Even though we've only implemented one function, since the others can defined using it we get all of them for free:

```haskell
yep :: Bool
yep = Worse < PrettyAverage
-- yep = True
```

```haskell
yep2 :: Bool
yep2 = Fine >= Worse
-- yep2 = True
```

```haskell
yep3 :: Bool
yep3 = Fine >= Fine
-- yep2 = True
```

```haskell
nope :: Bool
nope = Fine < Fine
-- nope = False
```

We also get to use any function that requires an `Ord` instance for free, like `sort` from `Data.List`, which has the following type signature:

```haskell
sort :: Ord a => [a] -> [a]
```

This means "if you pass me a list of any `a` which is orderable, I can return you a sorted list". Thanks!

```haskell
import Data.List

moods :: [SuperMood]
moods = [Fine, Fine, Worse, PrettyAverage]

sorted :: [SuperMood]
sorted = sort moods
-- sorted = [Worse, PrettyAverage, Fine, Fine]
```

Great job!

Still seems like hard work though, can we auto generate this typeclass too? Yes!

```haskell
data LazyMood = Sloppy
              | Ploppy
              | Poopy
              | Nicey deriving (Eq, Ord)

lazySorted :: [LazyMood]
lazySorted = sort [Nicey, Poopy, Ploppy, Sloppy]
-- lazySorted = [Sloppy, Ploppy, Poopy, Nicey]
```

Nice.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Data.Ord](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Ord.html)
