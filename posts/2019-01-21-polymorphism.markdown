---
title: Why The Hell Should I Care About Polymorphism?
tags: haskell
---

Let's talk about type signatures.

```haskell
hello :: String -> Int -> String
```

Pretty clear what is going on, right? Seems fine. What about this lad?

```haskell
wtflol :: (Show a, Num b) => a -> [a] -> b -> (a, b)
```

That's pretty unhelpful to be honest.

What are these letters doing in our types when we wanted, well, types?

Let's work up from a simple one to a stupid one and maybe learn something along the way. Good? Great.

### Probably the worst function ever

Here is a type signature for a mysterious function:

```haskell
spooky :: a -> a
```

Given any `a`, and knowing nothing about that `a`, what is the only thing that we can do with it?

If you guessed "just return it", then yes, you were right. It's the classic `identity` function.

```haskell
identity :: a -> a
identity a = a
-- identity 10 == 10
```

What do you think this function does?

```haskell
mystery :: a -> b -> a
```

Since we don't know anything about `b`, and the function returns an `a`, then all we can do is ignore the `b` altogether. This is called the `const` function, and is used when we have to want to make a `map` function do not very much at all.

```haskell
const :: a -> b -> a
const a _ = a
-- const "dog" 100 == "dog"
```

Hopefully the pattern you are seeing here is that if we know nothing about our variables, then there is actually very little we can do with them.

### Lists of mysterious things

A place most people come across polymorphism quite early on in their Haskell careers is in stuff like lists. Instead of one function that works on `List String`, and another one `List Int`, and another on `List (Maybe Tuple (Int, String))` we can use polymorphism to act upon the structure of the list itself but not the items inside.

With this in mind, and given that `[a]` means a `List` that is full of zero or more `a` values, what possible things could this function do to our `[a]`?

```haskell
thing :: [a] -> [a]
```

According to the function search engine [Hoogle](https://www.haskell.org/hoogle/?hoogle=%5Ba%5D+-%3E+%5Ba%5D) - not a huge amount. It lists four functions that match this signature - and two of them throw exceptions if passed an empty list so they're not the kind of functions that we like to spend our time with. This leaves:

1. `cycle` - this repeats the list of values infinitely.

2. `reverse` - reverses the order of the list.

The important thing is that since we don't know anything about what the hell `a` is, all the functions can do is mess around with the ordering a bit, as is their right, I suppose. How can we find out more about these `a` and `b` values without concretely specifying what they are?

### Constraints

If there's anything going on before the `=>` in a type signature, there's a good chance it's a `constraint`. These are rules that apply to each type they refer to. Here, we are using a constraint on `a`, that says "I don't care what `a` is, so long as it has a `Show` instance".

```haskell
showTheList :: Show a => [a] -> String
showTheList []       = ""
showTheList (a : as) = show a ++ ", " ++ showTheList as
-- showTheList [1,2,3] == "1, 2, 3, "
```

Therefore, the only thing we can really do to these `a` values is `show` them, as we don't know anything else about it.

We can do something similar with the `Num` typeclass (which is the typeclass of numbers).

```haskell
addStuffUp :: Num a => [a] -> a
addStuffUp []       = 0
addStuffUp (a : as) = a + addStuffUp as
-- addStuffUp [1,2,3] == 6
```

Here, the constraint means we don't mind which kind of number we are passed - since they all implement `+` so we can add them together.

We can have as many constraints as we like on our variables (often multiple ones on the same value, such as `(Show a, Ord a) =>` for things that can be sorted and eventually turned into strings) - and can use any typeclass to do the restraining.

I hope this makes sense. This started melting into something about IO testing, but I have decided to spin that into a separate post before this one spirals out of control, so let's stop for now. Thanks.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Polymorphism for Dummies](http://www.haskellforall.com/2015/10/polymorphism-for-dummies.html)

[Polymorphism in Haskell](http://andrew.gibiansky.com/blog/haskell/haskell-typeclasses/)
