---
title: Typeclasses - Contravariant
tags: haskell, typeclasses
---

Let's think about Predicates.

```haskell
newtype Preddy a = Preddy { getPreddy :: a -> Bool }
```

What the hell is this?

OK. So it's a `newtype`. What is basically does is let's us take some sort of value (here, any function from `a -> Bool`) and make a new type out of it so we can give it typeclasses and do magic shit to it.

So let's say we have a basic function that takes a number and let's us know whether that number is over 3.

```haskell
threePred :: Int -> Bool
threePred i = i > 3
```

Great!

To put this into a `newtype` (our `Preddy` one, to be exact) - we do this:

```haskell
overThree :: Preddy Int
overThree = Preddy threePred
```

The type has changed - the `a` in `a -> Bool` is `Int` so we get `Preddy Int`. We don't need to specify the `Bool` anywhere because that's sort of implicit in the `Preddy`-ness.

OK. I get it, you're worried. My function is gone for ever. Can I get it out again? Sure.

```haskell
isOverThree :: Int -> Bool
isOverThree = getPreddy overThree
```

`threePred` and `isOverThree` are completely the same thing. So now we're comfortable this `newtype` thing is basically doing nothing awful to our code, what it is good for?

Oh yeah. We said earlier. Because `Preddy` is a datatype of it's own we can make typeclass instances for it. Let's make a `Contravariant` instance, for no other reason than this being the name of the post and we're a long way into it without doing anything particularly useful.

What will we need for doing that then?

```haskell
Prelude> :i Contravariant
```

If are typing along with all this and get an error, please skip to the bottom of the page for some useful links, however if you have the `contravariant` library floating around or are just happy to take my word for it then GREAT! You'll see this:

```haskell
class Contravariant (f :: * -> *) where
  contramap :: (a -> b) -> f b -> f a
  (>$) :: b -> f b -> f a
  {-# MINIMAL contramap #-}
```

The key thing to look when starting out is this `MINIMAL` part - as it means the only functions we need to worry about for the time being.

So, `contramap` is it. What does this do?

It takes a function from `(a -> b)` and then it takes an `f b` and returns an `f a`. Sure.

What?

Are you out of your fucking mind? How do I turn `b` into `a` using `(a -> b)`?

Is this...a backwards functor? Can I implement undo on all my functions? Is the special magic Haskell sauce I've been waiting for?

So it turns out no.

```haskell
nameLength :: String -> Int
nameLength ""      = 0
nameLength (x: xs) = 1 + nameLength xs
```

So a regular `functor` is actually a `covariant functor` which I guess means forwards functor. When we map over it, we changing what happens after it. Hence if I have a `Maybe String` and `fmap` a `String -> Int` function like `nameLength` over it I get a `Maybe Int` and all is well in the world.

However our pal `Preddy`, unbeknownst to him, is about to become a `contravariant functor` which sort of means backwards. What this means is if he's waiting for an `Int` to see if it's over 3, we can `contramap` that same `String -> Int` function `nameLength` over it, and instead `Preddy` is waiting for a `String` so he can tell you whether the `String` has over 3 characters. The map happens beforehand, basically. That's how an `(a -> b)` function turns a `Preddy b` (predicate waiting for a `b`) into a `Preddy a` (predicate waiting for an `a`).

Seems weird? Sure. Let's look at an example.

```haskell
instance Contravariant Preddy where
  contramap f (Preddy p) = Preddy (p . f)
```

This looks a lot like `fmap`, except our new `f` function happens BEFORE the `p` function that is already in there.

```haskell
nameLengthOverThree :: Preddy String
nameLengthOverThree = contramap nameLength overThree
```

Here we've turned our `Preddy Int` function `overThree` into a `Preddy String` function using a `String -> Int` function and `contramap`.

Let's use it!

```haskell
nameIsOverThree :: String -> Bool
nameIsOverThree = getPreddy nameLengthOverThree
-- nameIsOverThree "Lou" == False
-- nameIsOverThree "Doug" == True
```

Great! Now we can use it to see if words are Just Too Long.

All this wrapping and unwrapping seems a lot of work for that, but what if we start using `contramap` for more?

```haskell
data Person = Person { name :: String, age :: Int } deriving (Show)

steve :: Person
steve = Person { name = "Steve", age = 100 }

lou :: Person
lou = Person { name = "Lou", age = 69 }
```

Here are Steve and Lou. They are, at least syntactically, people.

Let's use mathematics to judge whether their names are too long.

```haskell
personTooLong :: Person -> Bool
personTooLong = getPreddy personPreddy where
  personPreddy = contramap (nameLength . name) overThree

-- personTooLong steve == True
-- personTooLong lou == False
```

Excellent! That will show them.

You will note that here we've somewhat rushed and combined the `contramap`-ing and unwrapping into one function. This was mostly to show that when you come to use these things, it doesn't need to be quite as laborious as our broken down examples above. Hopefully you can follow, the key thing is that `(nameLength . name)` means "put the value into `name` and the pass the result to `nameLength`".

Contravariant doesn't show up a huge amount on it's own, but it comes into it's own as part of `Profunctor` (lightning flashes, thunder, excitement!) which no doubt we'll flop towards at some unspecified future moment.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[The Extended Functor Family (video)](https://www.youtube.com/watch?v=JZPXzJ5tp9w)

[Data.Functor.Contravariant](http://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant.html)

NOTE: If you happened to get this error:

```haskell
<interactive>:1:1: error: Not in scope: ‘Contravariant’
```

...then you will need to install the `contravariant` package from Hackage. I'm not going to go into Haskell package management at this point but here is a good starting point: [Practical Haskell - Getting Started](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html). It's all going to be fine, I promise. The documentation isn't great but the tools are pretty nice.
