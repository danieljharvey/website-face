---
title: Typeclasses - Eq
---

Let's think about horses. There are three kinds of Horse:

```haskell
data Horse = SmallHorse | LargeHorse | OtherHorse
```

Let's make a function to check whether two Horses are in fact equivalently sized.

```haskell
isSameHorse :: Horse -> Horse -> Bool
isSameHorse first second = first == second
```

Looks like a classic. Let's run it!

```haskell
isSameHorse SmallHorse LargeHorse
```

Shit!

```haskell
• No instance for (Eq Horse) arising from a use of ‘==’
• In the expression: first == second
    In an equation for ‘isSameHorse’:
        isSameHorse first second = first == second
```

That's terrible news. What's wrong here? Apprently, we need to make an instance of the `Eq` (short for 'equality') typeclass for `Horse` before they can be compared. What's the `Eq` typeclass?

We can find out more by firing up `ghci`, the GHC repl.

You should see a prompt with the following:

```bash
Prelude>
```

If we enter `:info Eq`, we get the following:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

It shows there are two functions in the `Eq` typeclass, `==` and `/=` (equals and not-equals), and that a "minimal" definition of `Eq` only needs one of those.

Let's start again and make a better horse.

```haskell
data BetterHorse = Tiny | Average | Huge
```

Let's not make the same mistake this time - let's make an instance of the `Eq` typeclass for them. We are going to implement `==` which has a type of `a -> a -> Bool`.

```haskell
instance Eq BetterHorse where
    Tiny == Tiny = True
    Average == Average = True
    Huge == Huge = True
    _ == _ = False
```

OK, seems fine. We've listed all the times two `BetterHorse` are the same and used `_ == _ = False` to mean "anything else is not equal" to save ourselves listing every alternative.

```haskell
isSameBetterHorse :: BetterHorse -> BetterHorse -> Bool
isSameBetterHorse first second = first == second
```

Now our `BetterHorse` comparing function works. Let's give it a go.

```haskell
nope :: Bool
nope = isSameBetterHorse Tiny Huge
-- nope = False
```

```haskell
yep :: Bool
yep = isSameBetterHorse Average Average
-- yep = True
```

All seems to be fine here. We even get the `/=` function for free by defining `==`.

```haskell
nice :: Bool
nice = Average /= Tiny
-- nice = True
```

If you're thinking "this seems laborious", you'd be right. Fortunately, for basic data types like this, we can simply auto-generate an `Eq` instance in the data definition like this:

```haskell
data LazyHorse = LazyTiny | LazyOther deriving (Eq)
```

```haskell
workingNow = LazyTiny == LazyTiny
-- workingNow == True
```

Great!

Further reading:

[Data.Eq](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Eq.html)
