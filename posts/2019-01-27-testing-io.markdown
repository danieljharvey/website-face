---
title: IO, IO, it's off to test we go
tags: haskell, testing
---

Let's say we have a function that uses the `IO` monad that we want to test. Now you may be aware that `IO` is the place in which all the bad things happen that aren't supposed to happen in Haskell. Things like *Database Connections*, and *Mutable Global Variables* and (worst of all) *Actual User Interactions*. It doesn't bear thinking about to be honest.

However, mock dramatics aside, `IO` is the part of our code where *Side Effects* happen, meaning stuff is a lot more difficult to write tests for, given our functions may not always return the things we expect them to. Last time, we spoke about [polymorphism](/posts/2019-01-21-polymorphism.markdown) - and this seems like a nice opportunity to expand on that and use our new polymorphic powers to make some code easier to test.

### Contrived example

Here is some code that uses `getCurrentTime` from the `Data.Time.Clock` library to fetch the current system time and then work out whether it is time to stop working and eat some lunch.

```haskell
import qualified Data.Time.Clock     as Clock
import qualified Data.Time.Calendar  as Cal
import qualified Data.Time.LocalTime as Time

getHour :: Clock.UTCTime -> Int
getHour = Time.todHour
        . Time.timeToTimeOfDay
        . Clock.utctDayTime

isItLunchTime :: IO Bool
isItLunchTime
  = lunchCheck <$> getHour <$> Clock.getCurrentTime
    where
      lunchCheck hr = hr >= 12 && hr <= 14
```

The `getHour` function is a simple pure function helper - mainly we'll be looking at `isItLunchTime`. Let's look at the type signature again:

```haskell
isItLunchTime :: IO Bool
```

This function has no inputs, and the `IO Bool` return type means that it will return `True` or `False`, wrapped in the `IO` type to remind us that evil has been done and the gods of referential transparency have not been appeased.

If you are thinking something like "can we not just extract the pure functions, test those, and not bother about the rest?", then Don't Read On because although that's Not A Terrible Idea we're absolutely Not Doing That.

### Injection-based solution

In other languages, the solution to making stuff more testable is to use `Dependency Injection` to pass in any functions that use side effects, allowing mock versions to be passed in when testing.

This version of the function lets us pass in a time fetching function (such as `getCurrentTime`) instead.

```haskell
injectableLunch :: IO Clock.UTCTime -> IO Bool
injectableLunch getCurrentTime
  = lunchCheck <$> getHour <$> getCurrentTime
    where
      lunchCheck hr = hr >= 12 && hr <= 14
```

Now, instead of calling `isItLunchTime`, we'd call `injectableLunch Clock.getCurrentTime`. It's still not much more testable though because of that `IO` though. Does it need to be `IO` though? What if we generalise the type signature though? Instead of `IO`, let's just use any old `Monad` and see if that works.

```haskell
testableLunch :: (Monad m) => m Clock.UTCTime -> m Bool
testableLunch getCurrentTime
  = lunchCheck <$> getHour <$> getCurrentTime
    where
      lunchCheck hr = hr >= 12 && hr <= 14
```

Because we're not actually running any functions that require the `IO` monad (ie, `readFile`, `putStrLn`) any monad is totally fine for this function. So long as the Monad `m` of our passed in time-getting function matches the Monad `m` of the return value, then this function is having a Great Time.

As a slight detour, let's make some fake `UTCTime` values for testing:

```haskell
baseTestTime :: Clock.UTCTime
baseTestTime
  = Clock.UTCTime
      { Clock.utctDay     = Cal.ModifiedJulianDay 12000
      , Clock.utctDayTime = 0
      }

lunchTestTime :: Clock.UTCTime
lunchTestTime
  = baseTestTime { Clock.utctDayTime = 44000 }
```

Excellent. What a nice couple of examples. For more information about what these do, check out the [time](http://hackage.haskell.org/package/time-1.9.2) library.

Now we have some test values, we can then make functions to test our `testableLunch` function with a safer Monad like `Identity`. `Identity` is the Do Nothing monad, it has no special characteristics and does nothing for any particular interest.

```haskell
testNotLunch :: Identity Bool
testNotLunch = testableLunch (pure baseTestTime)
-- Identity False
```

```haskell
testIsLunch :: Identity Bool
testIsLunch = testableLunch (pure lunchTestTime)
-- Identity True
```

(What's `pure`? It's just a function that wraps a value inside the default Monad context, so in this case, turns `"dog"` into `Identity "dog"` etc)

Then when we want to export the function for use with `IO`, we can export this:

```haskell
isItLunchTime3 :: IO Bool
isItLunchTime3 = testableLunch Clock.getCurrentTime
```

Seems pretty OK. What about a more idiomatic Haskell approach using our pal typeclasses?

### Testing IO with a typeclass

Another way to do this is capture the idea of "Monad That Knows What The Time Is" using a typeclass.

Let's call ours `MonadTime`, because we lack imagination:

```haskell
class Monad m => MonadTime m where
  getTheTimePlease :: m Clock.UTCTime
```

Note the constraint - we can only make instances of `MonadTime` for things that are already an instance of `Monad`. By writing instances of `MonadTime` for `IO` and `Identity`, we can tell Haskell what to do when it's asked for the time.

```haskell
instance MonadTime Identity where
  getTheTimePlease = pure lunchTestTime

instance MonadTime IO where
  getTheTimePlease = Clock.getCurrentTime
```

`Identity` will return a static value, and `IO` will use the actual function.

Here's our new version of the lunch function. Notice we've changed our constraint from `Monad m`

```haskell
classyLunch :: (MonadTime m) => m Bool
classyLunch
  = lunchCheck <$> getHour <$> getTheTimePlease
    where
      lunchCheck hr = hr >= 12 && hr <= 14
```

Now, depending on the context in which we use `classyLunch`, it'll do different things.

```haskell
testClassyLunch :: Identity Bool
testClassyLunch = classyLunch

ioClassyLunch :: IO Bool
ioClassyLunch = classyLunch
```

Hooray! Our contrived function is now testable and also ready for production. Great stuff.

This concept of different typeclasses for Monads is a key part of what is called `mtl` style of code, which we will come to once we've looked at Monad Transformers.

Further reading:

[Time library](http://hackage.haskell.org/package/time-1.9.2)

[Better IO testing through Monads](https://commentedcode.org/blog/2017/08/21/better-io-testing-through-monads/)
