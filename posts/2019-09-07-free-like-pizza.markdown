---
title: Free like pizza, not like jazz
tags: haskell, free, monad
---

When I first started working at a *Serious Software Place*, I remember being very excited about the idea of testing things and dependency injection and purity and all such things. One day, I was spouting about this (presuming, I suppose, that everybody else was also so new to and thus enamoured with the topic), as well as these `Functors` I had been reading about, and one of the more senior engineers said, "Well *(raises eyebrows suggestively)*...you're going to **LOVE** Free Monads!".

Now, many people that know me would agree I am a fairly jaded man, but I have to admit that even my interest was pigued. However, it's taken me absolutely fucking ages to work out what they are and how they relate to this original promise, and I *think* they do, just not how I thought.

### The Intuition For Free Monads

So, the thing that took me the longest time to lose was this assumption I had made that a `Free Monad` was a wild monad, from the edges of abstract math. It's Ornette Coleman! It's John Zorn! It's a saxophone played with a hoover! It can do anything, at any time! Needless to say, this was a very exciting idea, so I set out to read about, framing everything I read with this wild (but admittedly very attractive) mis-assumption. Needless to say, I was very confused and wasted a lot of time that I could have spent doing fun things instead.

The reality is that the `Free` in `Free Monad` is not free like *"jazz"* or *"thinking"*, but free as in *"pizza, at a tech meetup, knowing that of course the real price is listening through the following recruitment messages"*. They are still very good, but *(in my humble opinion, etc)*, not half as fun as I had originally promised myself they would be.

With this crushing piece of disappointment aside, let's look at what they actually are.

### Dogs Disguised As People By Way Of Wearing A Coat And Hat

![Bear with me here.](/images/dogs-in-coat.png "Bear with me here.")

Let's start with the idea of a `Functor`. If this is confusing to you I have written a [vague explanation](http://localhost:8000/posts/2018-11-16-typeclasses-functor.html) and so have much more [competent](http://www.tomharding.me/2017/03/27/fantas-eel-and-specification-6/) [people](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html) which are probably worth digesting before we go on.

A functor is a data structure which contains some other data, and we can "map over" the data inside it to change that without fucking up the functor itself. For example, we can use the `map` function on a `Maybe` value, and it will change the `a` on a `Just`, and do nothing on `Nothing`.

```haskell
data Maybe
  = Just a
  | Nothing

fmap (+1) (Just 1) -- Just 2
fmap (+1) Nothing  -- Nothing
```

Now, there is already a `Monad` instance for `Maybe`, and this means we can use `do notation` and all sorts of great stuff with it, like this:

```haskell
-- get the first item of the first list
-- and the first item of the second list
-- and then smash them together using append
-- from their semigroup instance
firstAndSecondCombine 
  :: (Semigroup a) 
  => [a] 
  -> [a] 
  -> Maybe a
firstAndSecondCombine firstList secondList = do
  a <- first' firstList
  b <- first' secondList
  pure (a <> b)

-- helper that returns the first item of a list if it exists
first' :: [a] -> Maybe a
first' stuff 
    = case stuff of
        (x:_) -> Just x
        _     -> Nothing
```

Let's try them with the `Product` monoid *(which multiplies its contents together when combined)*, to satisfy ourselves that this function does a thing that we vaguely like and approve of.

This fails as it should....

```haskell
nah :: Maybe (Product Int)
nah = firstAndSecondCombine [] []
-- nah == Nothing
```

...and this gives us an answer.

```haskell
yeah :: Maybe (Product Int)
yeah = firstAndSecondCombine 
          [Product 10, Product 11]
          [Product 20, Product 5]
-- yeah == Just (Product 200)
```

*(Isn't `Monoid` such a nice thing?)*

### That Didn't Mention Dogs Or Hats, What Gives?

Ahh shit, yes.

So.

What if there isn't a `Monad` instance? Or we want to write programs like this, but don't want to commit to using `Maybe`? (or perhaps `Maybe` is **TOO** safe, and we want the option of throwing loads of exceptions instead). Could this be a job for...a `Free Monad`?

*(note for readers with subtext disabled in their browser: yes)*

OK. Let's make a datatype that's like `Maybe`, but isn't.

```haskell
data MoybeF a
  = Jost a
  | Nothong
```

Let's make an instance of `Functor` for it to prove there is no funny business at hand.

```haskell
instance Functor MoybeF where
  fmap _ Nothong  = Nothong
  fmap f (Jost a) = Jost (f a)
```

Map the `f` function over an `a` if there is one, if not, do nothing. Good?

### Still No Dogs, What Gives?

Right. In this example, `MoybeF` is our dog. It's a plain old functor that doesn't really do anything except contain an `a` inside `Jost`, or contain nothing at all inside `Nothong`. What `Free` does for us (and by that, I mean `Free` from [Control.Monad.Free](http://hackage.haskell.org/package/free-3.3.1/docs/Control-Monad-Free.html)) is gives us a coat to wrap about this `Functor` to let us use it like a `Monad`.

*(In this tenuous analogy, the `Monad` represents a human adult, or at least some impression of one)*

```haskell
-- Free turns MoybeF (a mere functor)
-- into Mobye (a mighty monad)
type Moybe a = Free MoybeF a
```

*(An aside: this metaphor felt a lot more powerful in my mind, but a cursory image search came up with a lot less than I had expected, and quite frankly this picture creeps me the fuck out).*

![You deserve better than this.](/images/free-structure.png "You deserve better than this.")

OK. So now, by use of the `liftF` functor from `Control.Monad.Free`, we can change any `MoybeF` value into a `Moybe` monad value instead. Let's define some helper functions that do this:

```haskell
-- lift a into the Moybe monad
jost :: a -> Moybe a 
jost = liftF . Jost

-- create an empty Moybe value
nothong :: Moybe a
nothong = liftF Nothong
```

Now, we can redefine our function from above to our our new exciting `Moybe` type too!

```haskell
-- get first item (if there is one)
fFirst' :: [a] -> Moybe a
fFirst' stuff 
    = case stuff of
        (x:_) -> jost x
        _     -> nothong
```

(We could just have used `liftF` directly, and avoided the helpers, but this way hopefully we can see better how equivalent the two `first'` functions are)

Now, let's remake our list smashing function from earlier using `Moybe`....

```haskell
fFirstAndSecondCombine 
  :: (Semigroup a) 
  => [a] 
  -> [a] 
  -> Moybe a
fFirstAndSecondCombine as bs = do
  a <- fFirst' as
  b <- fFirst' bs
  pure (a <> b)
```

Hopefully, you can see it's pretty much the same. But how do we use it? If we run `fFirstAndSecondCombine [] []`, for instance, what we get back is a `MoybeF` again. So what the hell use is that? I went to all of this trouble to create a `Monad` (admittedly as cheaply as possible) - but it doesn't do anything!

### Interpreting The Computation

So what is `MoybeF`? Essentially, it's a data structure describing a monadic computation. Therefore, if we want to turn into something that does things, we need to *interpret* it.

Let's turn it back into our original `Maybe` computation:

```haskell
interpretMaybe :: Moybe a -> Maybe a
interpretMaybe
  = foldFree interpret
  where
    interpret :: MoybeF a -> Maybe a
    interpret prog'
      = case prog' of
          Jost a  -> Just a
          Nothong -> Nothing
```

As you can see, we're just substituting `Jost` for `Just` and `Nothong` for the inferior `Nothing`.

Now we can use our function to actually do things like before:

```haskell
ans2 :: Maybe (Product Int)
ans2 = interpretMaybe (fFirstAndSecondCombine [Product 10] [Product 5, Product 20])
-- ans2 == Just (Product 200)

ans3 :: Maybe (Product Int)
ans3 = interpretMaybe (fFirstAndSecondCombine [] [Product 5, Product 20])
-- ans3 == Nothing
```

### I See That Once Again We're Doing Something Easy The Difficult Way

OK, fair point. Where is the value in all this indirection?

How about if we want to interpret this computation differently? Perhaps we need to return an `Either` instead? Changing the interpreter is pretty straightforward:

```haskell
interpretEither :: Moybe a -> Either String a
interpretEither
  = foldFree interpret
  where
    interpret :: MoybeF a -> Either String a
    interpret prog'
      = case prog' of
          Jost a  -> Right a
          Nothong -> Left "Nah, that didn't work"

ans4 :: Either String (Product Int)
ans4 = interpretEither (fFirstAndSecondCombine [Product 10] [Product 5, Product 20])
-- ans4 == Right (Product 200)

ans5 :: Either String (Product Int)
ans5 = interpretEither (fFirstAndSecondCombine [] [Product 5, Product 20])
-- ans5 == Left "Nah, that didn't work"
```

### Is This...A Domain Specific Language?

That's right. One of the main strengths of `Free` is in creating small little sub-languages for writing logic, that can be interpreted in a number of different ways (such as *"what even is this doing?"* and *"i don't understand this code"*)

### How Does This Relate To Testing?

So, this example is pointedly very simple indeed, and involve swapping one pure thing for another, but a much more common use of this pattern is to take some code that does effectful things (like save files, write to a database, etc) and write them in a way where they can be run in the program using `IO`, but then testing by turning them into something pure like `Writer`, and looking at the output.

### Do You Have An Example Of Exactly That, Preferably Centered About Reading And Writing To A Terminal Window, Perchance?

Why yes, I do.

So `ConsoleF` is a `Functor` with two commands for input and output from a terminal.

```haskell
data ConsoleF next
  = Write String next
  | Read (String -> next)

instance Functor ConsoleF where
    fmap f (Write s next) = Write s (f next)
    fmap f (Read next)    = Read (f . next)
```

We power it up into a `Free Monad`...

```haskell
type Console a = Free ConsoleF a
```

...and then we write our helpers with `liftF`...

```haskell
-- write to the console
fWrite :: String -> Console ()
fWrite str = liftF $ Write str ()

-- await user input
fRead :: Console String
fRead = liftF $ Read id
```

...then we write our program with it...

```haskell
consoleProg :: Console ()
consoleProg = do
    fWrite "What is your name?"
    a <- fRead
    fWrite $ "Sure? " ++ a
    b <- fRead
    fWrite "Great."
```

...then we interpret in `IO` to make a working program...

```haskell
interpretIO :: Console a -> IO a
interpretIO
  = foldFree interpret
  where
    interpret :: ConsoleF a -> IO a
    interpret prog'
      = case prog' of
          Write s a -> Prelude.putStrLn s >> pure a
          Read a    -> a <$> Prelude.getLine
```

...then we interpret it in `Writer` for testing...

```haskell
interpretWrite :: Console a -> Writer [String] a
interpretWrite = foldFree interpretConsoleWrite
  where
    interpretConsoleWrite prog'
      = case prog' of
          Write s a -> do
            _ <- tell [s]
            pure a
          Read a -> do
            _ <- tell ["[wait for input]"]
            pure (a "[user input]")
```

...run it...

```haskell
output :: [String]
output = (snd . runWriter . interpretWrite ) consoleProg
-- output == ["What is your name?","[wait for input]","Sure? [user input]","[wait for input]","Great."]
```

...and we go home for a nice sit down.

### Isn't This The Point Of Typeclasses?

Aye, [some would say so](https://markkarpov.com/post/free-monad-considered-harmful.html). But what's the point of Haskell if there isn't 10 ways of doing the same thing, and endless bloody fights to the death about the minor differences between them?

### That Is True, You Are So Right. Any Other Helpful Links?

The thing that finally made all this click for me was this video by [Nate Faubion](https://www.youtube.com/watch?v=eKkxmVFcd74). Once I did understand it wasn't about jazz, this by [Matt Parsons](https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html) was really great too.