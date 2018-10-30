---
title: Do Notation
---

Do notation is a strange thing. You first see it in examples of `IO ()` with something like

```haskell
main :: IO ()
main = do
    firstName <- getStrLn
    surname <- getStrLn
    let fullName = firstName ++ " " ++ surname
    print ("Hello " ++ fullName)
```

And all seems well! This functional programming seems fine! But what's going on here?

Actually what's really going on here is

```haskell
main :: IO ()
main = do
    getStrLn >>= (\firstName -> getStrLn >>= (\surname -> print ("Hello " ++ fullName))) where
        fullName = firstName ++ " " ++ surname
```

What's this mean then?

So `getStrLn` is a function that returns an `IO String`, which means `a string wrapped in the IO monad`. With a bit of hand-waving, the `>>=` function runs it, unwraps the `String` inside and passes it into the anonymous function (the one starting with `(\firstName -> ...)`). This function runs it AGAIN and passes the unwrapped result into the next function. As you can see, the layers could get quite deep, hence the do notation which aims to make everything clearer.

So is it just an `IO` thing?

No!

Here's an example with Maybe:

```haskell
main :: [[a]] -> Maybe a
main = do
    as <- safeHead
    a <- safeHead as
    return a
```

What does this do? Well it takes a list of lists, and returns the first value of the first list inside. Therefore there are two potential points of failure, that being, the entire input being an empty list, or the first list inside the main input being an empty list. Let's desugar it!

```haskell
main :: [[a]] -> Maybe a
main aas = safeHead aas
         >>= (\as -> safeHead as
         >>= (\a -> return a))
```

There is probably a shorter way to write this one to be fair.

```haskell
main :: [[a]] -> Maybe a
main = safeHead >>= safeHead
```

OK, so in this case, `do notation` doesn't look so great. Oh well. Remember: clever things are interesting but they're not always good.
