---
title: Typeclasses - Bifunctor
tags: haskell, typeclasses
---

Let's talk about things that could be one thing, or indeed could be another thing altogether.

```haskell
data Things a b = This a | That b
```

This could be a `This` with an `a` inside, like a `String`.

```haskell
thisEgg :: Things String Int
thisEgg = This "Egg"
```

Or indeed a `That` with a `b` inside, like an `Int`.

```haskell
thatNumber :: Things String Int
thatNumber = That 68
```

Now hopefully you are now thinking - "Oh please, I do hope we map a function over one of these values soon" - and worry not, we absolutely bloody can.

### Mappity Mappity Map

Let's really push the boat out, and add one to the value inside.

First we'll need a [Functor](/posts/2018-11-16-typeclasses-functor.markdown) instance. Hopefully nothing too surprising here.

```haskell
instance Functor (Things a) where
  fmap f (That b) = That (f b)
  fmap _ (This a) = This a
```

Now we can map away to our heart's content:

```haskell
biggerNumber :: Things String Int
biggerNumber = fmap addOne (That 68)
-- biggerNumber == That 69
```

Nice.

But what about `This "Egg"``? I'd like to get at that egg. Perhaps eat it.

```haskell
eat :: String -> String
eat s = "The " ++ s ++ " was delicious!"
```

Can we do that with `Functor`?

```haskell
doesntWork :: Things String Int
doesntWork = fmap eat thisEgg
-- ERROR: Couldn't match type ‘[Char]’ with ‘Int’
```

I'm afraid not. Looking back at our `Functor` instance we can see that the `fmap` function only lets us map over the values inside `That`, leaving poor `This` and our lonely egg very much map-less. But fear not! We have another weapon at hand that will let us get at it.

Enter....`Bifunctor!`

(cue lightning, thunder, explosions and sounds of a large crowd who are clearly quite impressed).

### Definition

Let's ask `ghci` what's up.

```haskell
> import Data.bifunctor
> :i Bifunctor
```

```haskell
class Bifunctor (p :: * -> * -> *) where
  first :: (a -> b) -> p a c -> p b c
  second :: (b -> c) -> p a b -> p a c
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  {-# MINIMAL bimap | first, second #-}
```

Ok. Three functions in here, and it looks like we can make something a `Bifunctor` by implementing instances of both `first` and `second` or just `bimap`.

Let's take a look at them.

1. `first :: (a -> b) -> p a c -> p b c` - this takes a `Bifunctor` that may contain some `a` and `c` values, and a function that turns an `a` into some sort of `b`. It then runs the function on the `a` value, turning it into a `b` value. Sort of like doing an `fmap` over the `a` inside `This` from earlier. Pretty nice. Tl;dr - it's `fmap` but over the left value.

2. `second :: (b -> c) -> p a b -> p a c` - this takes a `Bifunctor` with an `a` and a `b` and a function that turns the `b` values into `c` values. In the case of our `Things` datatype of `This` and `That`, this let's us get at the `That` values, which we could anyway so big whoop. Tl;dr - it's our pal `fmap` again.

3. `bimap :: (a -> b) -> (c -> d) -> p a c -> p b d` - this takes a `Bifunctor` that may contain `a` and `c` values, and runs a function over both sides. It's doing `first` and `second` at the same time.

OK. If you understand `Functor` there's hopefully nothing out of the ordinary going on here. Let's slop an instance together and get to work on that delicious egg.

### Instances

Laziness dictates that we should define `bimap` because it is one function instead of two.

```haskell
instance Bifunctor Things where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)
```

Seems fairly sensible hopefully. Let's give it a spin.

```haskell
delicious :: Things String Int
delicious = first eat (This "Egg")
-- delicious = This "The Egg was delicious!"
```

Hooray! Although we defined `bimap` we got `first` for free, and that egg was pretty nice.

We can still map over the right hand value too!

```haskell
doesWork :: Things String Int
doesWork = second addOne (That 68)
-- doesWork == That 69
```

Again, nice.

### Tuples, Pooples

Although our `Things` example is about `sum` types, we can also use it on `product` types like a `Tuple`, and use `Bifunctor` to mess with either value as we please.

```haskell
twoThings :: (Int, String)
twoThings = (100, "Dogs")
```

Now, we could go ahead and show you `first` and `second` but I think you can work out what's going to happen, so let's go absolutely bonkers and race straight to `bimap`.

(but first, a helper function. Nothing untoward - it merely returns the first thing you give it and ignores the second.)

```haskell
myConst :: a -> b -> a
myConst a _ = a
```

Now we can turn our `Tuple` into a bestselling novel.

```haskell
oneBestSeller :: (Int, String)
oneBestSeller = bimap (+1) (myConst "Dalmations") twoThings
-- oneBestSeller = (101, "Dalmations")
```

I Bet You Did Not See That Coming.

For a bonus point, why not try and define `first` and `second` for `Tuple` types using `bimap` and `myConst`? Go on. You'll have a great time, I absolutely promise.

### That's all, folks

So although helpful with `Tuple` and `Either` types, `Bifunctor` isn't particularly mindblowing, but comes into it's own when we combine it with [Contravariant](/posts/2018-11-17-typeclasses-contravariant.markdown) to make `Profunctor`. More on that in the future though!

Further reading:

[The Extended Functor Family (video)](https://www.youtube.com/watch?v=JZPXzJ5tp9w)

[Data.Bifunctor](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Bifunctor.html)
