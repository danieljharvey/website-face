---
title: Testing in the IO monad
tags: haskell
---

Let's try a more interesting example to test some `IO` functions.

### IO, IO, it's off to work we go

Let's say we have a function that uses the `IO monad` that we want to test. Now you may be aware that `IO` is the place in which all the bad things happen that aren't supposed to happen in Haskell. Things like *Database Connections*, and *Mutable Global Variables* and *User Interactions*. It doesn't bear thinking about to be honest.

This is the place where *Side Effects* happen. *Side Effects* are things that mean our functions may not always return the same things every time, even if given the same inputs. Terrible stuff. Urgh.

Anyway, code with *Side Effects* is not code we want to test, so why don't we use our new polymorphic special powers to make some code easier to test?

Great!

```haskell
isItLunchTime :: IO Bool
isItLunchTime
  = lunchCheck <$> getHour <$> Clock.getCurrentTime
    where
      lunchCheck hr = hr >= 12 && hr <= 14
```

Using the current system time, this function tells us whether it is lunch time. The `IO Bool` return type means that it will return `True` or `False`, wrapped in the `IO` type to remind us that evil has been done and the gods of referential transparency have not been appeased.

You might be thinking something like "surely the key here is to extract the pure functions and test those individually and not worry about testing side effecting code" and sure yes, you are *technically* correct but please allow me this weak contrived example in the hope of making a more powerful point in the long term.

### Injection-based solution

### Testing IO with MonadTime typeclass

## end
