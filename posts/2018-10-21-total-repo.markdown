---
title: Total Repo
---

I recently read a piece of useful advice on Twitter about total functions which I enjoyed. A total function is a function that a) always returns some valid data (rather than just when it feels like doing so) b) does not go into an infinite loop. The advice (which I will definitely accurately source at some point in the future, lol) goes something like this:

#### "You start making your functions total by adding Maybe, but then you move onto removing Maybe."

Like all my favourite advice this sounds like it doesn't make any sense, so I figured I'd try and get to the bottom of this.

Let's start with a non-total function. The canonical example is Head from the Haskell Prelude, which returns you the first item in a list, and looks something like this:

```haskell
head :: [a] -> a
head (x: xs) = x
```

If we pass along a list like `[1,2,3]`, we get `1` back, all great.

But what if we pass it an empty list - `[]` ?

If you guessed "giant heartbreaking error"- then correct! Exactly the type of thing we were told would never ever happen again once we submitted to a life of functional purity. So what's the deal here?

Well, firstly, some history - this function was there when Haskell was still growing up and learning what to do in the world, and it's very difficult to go and change something that is used in a million libraries, so it's still there but should be left well alone. The Proper Way To Do Things is to use a total version of the function, which looks something like this:

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x: xs) = Just x
```

Now what happens? If we pass it `[1,2,3]` we get 'Just 1` back (the first item wrapped in a Maybe type). Great!

What about

```haskell
data NonEmptySteve a = Steve String a [a]
```
