---
title: Typeclasses - Functor
---

Let's think about things that might or might not happen.

```haskell
data Perhaps a = Yeah a | Nah
```

Perhaps is a datatype that we can also use as a container for other data (by holding it inside a nice `Yeah`), or to show that we have no data with `Nah`.

Let's put things in it.

```haskell
john :: Perhaps String
john = Yeah "John"
```

Great job. John is having a nice time there. How might we express an absence of John?

```haskell
nope :: Perhaps String
nope = Nah
```

OK. No Johns here.

So what if we have a function that receives something wrapped in a `Perhaps` and wants to do stuff with it.

I guess we have pattern matching, so we can use this to pull stuff out and do things with it. Let's add some question marks to this name, because that is always a valuable thing to do.

```haskell
questionAdd :: Perhaps String -> Perhaps String
questionAdd Nah         = Nah
questionAdd (Yeah name) = Yeah (name ++ "???")
-- questionAdd Nah  = Nah
-- questionAdd john = Yeah "John???"
```

Ok. Seems good. What about adding a simple exclaimation point instead? (Remembering of course that anybody more than one at any point is a sure sign of insanity.)

```haskell
exclaimAdd :: Perhaps String -> Perhaps String
exclaimAdd Nah         = Nah
exclaimAdd (Yeah name) = Yeah (name ++ "!")
-- exclaimAdd Nah  = Nah
-- exclaimAdd john = Yeah "John!"
```

Ok. Sure. So far, so functional. Feels like we're repeating ourselves though. What if we could abstract out away the unwrapping-and-then-wrapping-again and the function itself? Well sure we can! That typeclass is called `Functor`. Let's open ghci and take a look.

```haskell
Prelude> :i Functor
```

Here's the definition:

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```

Looks like we can implement it by just using `fmap`. And that type signature kind of looks like what we want (if we squint very hard indeed). It's saying take a function of any `a` to any `b` (ie, `(a -> b)`) and then pass me a functor with an `a` in it (ie, `f a`) and I will return the same functor but with the `b` from your function in it (ie, `f b`). So basically `fmap` is the "unwrapping-and-then-wrapping-again" we talked about earlier. Sounds good. Let's implement it.

```haskell
instance Functor Perhaps where
  fmap _ Nah      = Nah
  fmap f (Yeah a) = Yeah (f a)
```

Looks great. The first thing you'll notice is that the `fmap` for `Nah` doesn't do anything. That's because although our `Yeah` can contains some `a`, our `Nah` contains nothing at all so cares very little for our `a -> b` function. However, look! The second line is unwrapping the `a` from the `Yeah`, and then making a new `Yeah b` (because we make a `b` by running `f`, which is our `a -> b` function). Therefore we can throw any old function at this and we should have a Nice Time.

This function for instance, is used on a `String` to make the caller look somewhat unhinged.

```haskell
exclaim :: String -> String
exclaim str = str ++ "!!!!!!!!!!!!"
-- exclaim "Horse" == "Horse!!!!!!!!!!!!"
```

But using our new `Functor` instance means we can run it on our poor friend `john` from earlier, even though he is wrapped up in all that `Yeah`.

```haskell
veryJohn :: Perhaps String
veryJohn = fmap exclaim john
-- veryJohn == Yeah "John!!!!!!!!!!!!"
```

Sorry John.

What if we run the same function over a `Nah`? Does everything explode?

```haskell
stillNope :: Perhaps String
stillNope = fmap exclaim nope
-- stillNope = Nah
```

Nah. Seems fine. `Nah` ignores the function altogether, as it has no `a` in it, so no interest.

Ok. So this seems pretty great. There is one thing to know about `Functor` however that can be a little bit brain bending at first, and that's the idea that they can be "lawful". That is to say, that when you `fmap` over something, it doesn't also break that thing, and that it unwraps and wraps in the right way. Haskell's type system can help you make sure your `fmap` has the right types, but it can't enforce that your `Functor` makes sense I'm afraid. That's up to you.

The first law is called Identity. It means that if you `fmap` using an Identity function, nothing will change. This is a trick I suppose - the Identity function looks like this:

```haskell
identity :: a -> a
identity a = a
-- this is also called id in the Prelude
```

So running it on anything does nothing - the Identity law is basically checking the `Functor` is up to no funny business. Our Perhaps functor is OK, but what about this chancer?

```haskell
data Poohoops a = Yerp a | Nerp deriving (Eq, Show)

instance Functor Poohoops where
    fmap _ Nerp     = Nerp
    fmap f (Yeah a) = Nerp
```

If we `fmap identity Nerp` we get `Nerp`, so that all seems fine. However, if we `fmap identity Yeah "Detroit"` then we also get `Nerp` which is an absolute bloody disaster. This functor is broken. Put it straight in the bin, and set the bin on fire.

The second law is called Composition. It means that if we `fmap` one function over our `Functor` and then `fmap` a second function over the result, it would be the same as combining the two functions and doing a single `fmap`.

I'm sorry. What?

OK, let's have an example of that.

Therefore, if as well as our rather worrying "exclaim" function, we have one for shouting as well (don't be distracted by the `<$>` for now, we'll come to it in future...)

```haskell
capitalise :: String -> String
capitalise str = toUpper <$> str
-- capitalise "Horse" == "HORSE"
```

...we can combine it to make one terrifying function...

```haskell
shouting :: Perhaps String -> PerhapsString
shouting p = fmap (capitalise . exclaim) p
-- shouting (Yeah "Bruce") == Yeah "BRUCE!!!!!!!!!!!!"
```

...which is exactly the same as this:

```haskell
shouting2 :: Perhaps String -> PerhapsString
shouting2 p = fmap capitalise (fmap exclaim p)
-- shouting2 (Yeah "Bruce") == Yeah "BRUCE!!!!!!!!!!!!"
```

The Composition law just makes sure these are the same thing, so again, no funny business can take place.

```haskell
compositionLaw :: Bool
compositionLaw = fmap (capitalise . exclaim) (Yeah "Bruce")
              == fmap capitalise (fmap exclaim (Yeah "Bruce"))
-- compositionLaw == True
-- ie, either way of doing this ends up the same
```

Anyway. This is just one introduction to a kind of `functor`. In short, hand-wavey terms:

#### "A functor is a thing that lets you safely crap around with the values inside it without breaking the thing itself"

A few important notes and disclaimers:

1. There doesn't have to be just one `a` in the functor for this pattern to work. Another goto example for this is `List`, which can have absolutely loads in. In that case, the `fmap` runs the `(a -> b)` function on every item in the list, like `array.map` from Javascript, so it takes `[a]` and turns it into `[b]`.

2. There are functor instances for many datatypes such as `IO` where the `a` inside might represents a value that isn't there yet (like perhaps it will come from some user input etc). Therefore doing `fmap` on such a datatype is just saying "change the `a` inside to `b` whenever it happens to turn up", like "when the user types their name, change the string to have lots of exclamation marks so they look like a weirdo when we later print it back for them" or similar. This kind of idea can get a bit brain bending but once it settles it's sort of magical to know you can crap around with the future just like you're working with an array.

3) The `Perhaps` data type we have invented is really called `Maybe` and it's all over the place. More on that another time, perhaps.

That's quite enough for now.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Functors, Applicative and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
