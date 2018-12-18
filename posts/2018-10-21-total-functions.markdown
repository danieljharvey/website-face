---
title: Why The Hell Should I Care About Total Functions?
---

I recently read a piece of useful advice on Twitter about total functions which I enjoyed. The advice (which I will definitely accurately source at some point in the future, lol) goes something like this:

#### "You start making your functions total by adding Maybe, but then you move onto removing Maybe."

Like all my favourite advice this sounds like it doesn't make any sense, so I figured I'd try and get to the bottom of this.

Firstly, to clarify, a total function is a function that:

1.  Always returns some valid data (rather than just when it feels like doing so) and
2.  Does not go into an infinite loop.

Let's start with a non-total function. The canonical example is Head from the Haskell Prelude, which returns you the first item in a list, and looks something like this:

```haskell
head :: [a] -> a
head (x: xs) = x
```

If we pass along a list like `[1,2,3]`, we get `1` back, all great.

But what if we pass it an empty list - `[]` ?

If you guessed "giant heartbreaking error"- then correct! Exactly the type of thing we were told would never ever happen again once we submitted to a life of functional purity. So what's the deal here?

Well, firstly, some history - this function was there when Haskell was still growing up and learning what to do in the world, and it's very difficult to go and change something that is used in a million libraries, so it's still there but should be left well alone.

So what is the Proper Way To Do Things then?

It's to use a total version of the function, of course! Perhaps something like this:

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x: xs) = Just x
```

Now what happens? If we pass it `[1,2,3]` we get 'Just 1` back (the first item wrapped in a Maybe type). Great!

And what about if we use the empty list `[]` ? We get `Nothing`, which is certainly not an error.

So far, so total, great stuff.

However, our function is currently responsible for two things:

1.  Checking the inputs are even correct
2.  Getting the first value

Ideally, we should be able to use types to make sure this doesn't happen, and make validating that data Somebody Else's Problem.

Therefore, let's start by creating a new datatype...

```haskell
data NonEmpty a = NonEmpty a [a]
```

It contains a first value and a list, therefore ensuring there is always a first value. What does this to do to our head function then?

```haskell
nonEmptyHead :: NonEmpty a -> a
nonEmptyHead (NonEmpty x _) = x
```

Because the data type cannot be empty, our head function is guaranteed to return a value and is thus total.

But we've got a list! How do we get one of these new fangled data types? Let's make a helper function...

```haskell
createNonEmpty :: [a] -> Maybe (NonEmpty a)
createNonEmpty [] = Nothing
createNonEmpty (x : xs) = Just (NonEmpty x xs)
```

A function like this allows us to turn a regular list into a NonEmpty list, by returning a `Just NonEmpty` for a list with one or more items, or a `Nothing` if the original List is empty. This still means we have a possibility of failure (expressed by the Maybe), however our `head` function expects that to be dealt with elsewhere before it gets involved with the data, and is thus total whilst always returning a value.

Like all somewhat trivial examples, perhaps this seems like a lot of work for nothing (plus the overhead of dealing with a new datatype) - however this concept of doing a piece of validation once, capturing it in the type, and then having a much simpler time dealing with it onwards is very powerful indeed.

(btw, Haskell actually has a NonEmpty list type, in `Data.List.NonEmpty`, but it's more fun to make our own)

Anyway, that's quite enough for now I feel.

Further reading:

[Are your functions total?](https://adamdrake.com/are-your-functions-total.html)

[Data.Maybe](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html)

[Data.List.NonEmpty](https://hackage.haskell.org/package/semigroups-0.16.0.1/docs/Data-List-NonEmpty.html)
