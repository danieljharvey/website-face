---
title: Typeclasses - Show
---

Let's think about things.

```haskell
data Thing = Thing1 | Thing2
```

Seems great so far. Let's try and look at one in `ghci`.

```haskell
Prelude> show Thing1
```

Oh shit!

```haskell
• No instance for (Show Thing) arising from a use of ‘show’
• In the expression: show Thing1
    In an equation for ‘it’: it = show Thing1
```

Totally bogus!

So what's the problem? So for something to be shown in the console, we need to know how to convert it into a String. This is a pretty common thing to be able to do, so we have a typeclass for it, called `Show`. Let's check it out in `ghci`.

```haskell
Prelude> :i Show
```

```haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
```

A few more things than expected, tbh. Fortunately, that `{-# MINIMAL showsPrec | show #-}` means we can just define `show`, which takes any value of a type and turns it into a `String`.

Let's make another data type.

```haskell
data BetterThing = Better1 | Better2
```

This time, we'll create an instance of `Show` for it.

```haskell
instance Show BetterThing where
    show Better1 = "Better thing 1"
    show Better2 = "The other even better thing"
```

Now we can `show` it as much as we like.

```haskell
showBetter1 :: String
showBetter1 = show Better1
-- showBetter1 = "Better thing 1"
```

```haskell
showBetter2 :: String
showBetter2 = show Better2
-- showBetter2 = "The other even better thing"
```

Great! What if we don't need particularly clever string output? Just the name of the type would be fine. Can we cheat and derive one?

```haskell
data LazyThing = Lazy | Crazy | Other deriving (Show)
```

Nothing seemed to complain there. Good stuff!

```haskell
showLazy :: String
showLazy = show Lazy
-- showLazy == "Lazy"
```

Success! And, so, as one would expect...

```haskell
showCrazy :: String
showCrazy = show Crazy
-- showCrazy == "Crazy"
```

Anyway. That's enough of that.

Further reading:

[Text.Show](http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Show.html)
