---
title: Why The Fuck Should I Care About Do Notation?
---

Do notation is a strange thing. You first see it in examples of `IO ()` with something like:

```haskell
main :: IO ()
main = do
    firstName <- getLine
    surname <- getLine
    print ("Hello " ++ firstName ++ " " ++ surname)
```

And all seems well! This functional programming seems fine! But what's going on here?

Actually what's really going on here is

```haskell
main2 :: IO ()
main2 = getLine
    >>= (\firstName -> getLine
    >>= (\surname ->
        print ("Hello " ++ firstName ++ " " ++ surname)))
```

OK. That's a little scarier. Quite a lot of weird arrows and stuff. What's this mean then?

So `getStrLn` is a function that returns an `IO String`, which means `a string wrapped in the IO monad`. With a bit of hand-waving, the `>>=` (bind) function runs it, unwraps the `String` inside and passes it into the anonymous function (the one starting with `(\firstName -> ...)`). This function runs it AGAIN and passes the unwrapped result into the next function. As you can see, the layers could get quite deep, hence the do notation which aims to make everything clearer.

So is do notation it just an `IO` thing?

No!

Here's an example with Maybe. `safeHead` is a function that returns the first item of a list wrapped in a `Just`, which means that if there isn't a first item, instead of crapping out and ruining everything we return a `Nothing` which is easier to cope with.

```haskell
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a
```

And here is a function that does it twice with do notation. Note not an `IO ()` in sight!

```haskell
safeHeadTwice :: [[a]] -> Maybe a
safeHeadTwice aas = do
    as <- safeHead aas
    a <- safeHead as
    return a
```

What does this do? Well it takes a list of lists, and returns the first value of the first list inside. Therefore there are two potential points of failure, that being, the entire input being an empty list, or the first list inside the main input being an empty list. Let's desugar it!

```haskell
safeHeadTwice2 :: [[a]] -> Maybe a
safeHeadTwice2 aas = safeHead aas
            >>= (\as -> safeHead as
            >>= (\a -> return a))
```

There is probably a shorter way to write this one to be fair.

```haskell
safeHeadTwiceShort :: [[a]] -> Maybe a
safeHeadTwiceShort a = safeHead a >>= safeHead
```

OK, so in this case, `do notation` doesn't look so great. Oh well. Remember: clever things are interesting but they're not always good.

Further reading:

[Do Notation in Wikibooks](https://en.wikibooks.org/wiki/Haskell/do_notation)

[A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads)
