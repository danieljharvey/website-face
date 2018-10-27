---
title: Monoid, Mate
---

The thing I have found the most difficult to get my head around in the world of Haskell and categories etc is when I spend some time finding about a subject only to realise it's describing something I already understand in some way. I feel cheated! Why must I learn new words for this! And you're being so vague about it!

One such notion is that of the `Monoid` and it's weaker cousin `Semigroup` (Weaker? What? We'll come to it...)

I'll explain Monoid first, as although it's more complicated, it's way easier to find usable examples for it. Let's start with a couple of those, and then work out what the generalisation is.

A good one is strings.

Let's say we have two string: `"nice"` and `"dog"`. We can combine them using something like `concat`

```haskell
concat :: string -> string -> string
concat a b = a ++ b
```

This can be used thus: `concat "nice" "dog"` and will produce `"nicedog"`.

That's great for two strings, but what if we have a whole list of strings that we want to smash together into one giant magnificent string?

Enter foldr (fold right). The signature is a bit of beast, so let's break it down.

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = x `f` foldr f z xs
```

It actually takes three arguments:

1. A function with the type `a -> b -> b`.
2. The starting value for the fold of type `b`.
3. A list of values we wish to add together of type `[a]` - in our example this could be `["hot", "dog", "jumping", "frog", "albuquerque"]`.

If you squint, you're pretty much looking at the `array.reduce` function in Javascript.

(What is important to note when looking at signatures for functions like this is that `a` and `b` may happen to be the same type - the important thing here is that they don't have to be. When using a list of strings like we are, the type is actually `(string -> string -> string) -> string -> [string] -> string` which is simultaneously more and less confusing.)

Let's trying using it with our string `concat` function from earlier.

```haskell
stringList :: [string]
stringList = ["hot","dog","jumping","frog","albuquerque"]

output :: string
output = foldr concat "" stringList

-- output = "hotdogjumpingfrogalbuquerque"
```

Great! Hopefully this all makes sense - we passed `foldr` a function for combining strings and a starting value of an empty string. Why the starting value? Well, what would happen if we folded over an empty list?

```haskell
output2 = foldr concat "" []

-- output2 = ""
```

Here, the starting value also doubles up as a default for empty lists. Could we use just anything though? It would be kind of nice to have a sort of error value as a default, like this:

```haskell
output2 = foldr concat "sorry, no value" []

-- output2 = "sorry, no value"
```

However this wouldn't work when we DO have values:

```haskell
stringList :: [string]
stringList = ["hot","dog","jumping","frog","albuquerque"]

output :: string
output = foldr concat "sorry, no value" stringList

-- output = "sorry, no valuehotdogjumpingfrogalbuquerque"
```

I guess we need empty values that don't mess up the result then.

## What's Monoid then?

#### A Monoid is a way of generalising over the idea of combining things

It needs two things:

1. An operation for combining two values (called `mappend` - Monoid Append, I guess)
2. An empty value to start from (called `mempty` - Monoid Empty..?)

For strings the combining operation is our `concat` and our empty value is `""`. The key observation here is that adding `""` to the start or end of a string changes nothing about the string.

String isn't the only Monoid in our example either. The other could be the list itself, which has the following types:

```haskell
mappend :: [a] -> [a] -> [a]
mempty :: []
```

Using `concat` on two lists will smash one after the other, ie `concat [1,2,3,4] [5,6,7]` would make `[1,2,3,4,5,6,7]`. The key to identifying a good empty element is this:

#### If you concat the empty element onto a Monoid, the Monoid remains the same

Some other more straightforward Monoids apply to numbers. Integers, for instance, can form two Monoids. One is addition:

```haskell
mappend :: Int -> Int -> Int
mappend a b = a + b

mempty :: Int
mempty = 0
```

Adding `0` onto a number achieves nothing whatsoever.

However, for multiplication, things are similar but a bit different:

```haskell
mappend :: Int -> Int -> Int
mappend a b = a * b

mempty :: Int
mempty = 1
```

Multiplying something by zero would make it into zero, and that's certainly not 'doing nothing' - therefore here our empty value is `1`.

Anyhow - what does this information get us then?

It gets us `fold`!

```haskell
fold :: Monoid m => [m] -> m
fold = foldMap id
```

OK, that's great, `id` is the function that does nothing, but what's that `foldMap` doing? Good question:

```haskell
foldMap :: Monoid m => (a -> m) -> [a] -> m
foldMap f as = foldr (mappend . f) mempty as
```

It's our old pal `foldr` from a few paragraphs back, but using whichever `mappend` and `mempty` are attached to the Monoid `m`.

It also allows passing a function `f` which we can use to turn an non-Monoid `Int` into a Monoid `Sum` or `Product`, but more on that later, `id` is fine for now.

(A quick interlude for the confused, the `Monoid m =>` part of the type signature is not the first function argument, but rather a constraint saying 'all `m` in this type signature must be of the typeclass Monoid. The `f` is the function of type `(a -> m)`, and `as` are the array of Monoid values we are working on. The key here is the `=>` meaning 'constraint', rather than `->`, meaning function application. I know, I'm sorry.)

Therefore, we can just combine our list of strings like thus: `fold ["horse","time","great"]` to make `"horsetimegreat"`. What is the point in this? Well, when doing something simple like this, perhaps not much. We have saved typing around ten letters in exchange for making the function of our code less opaque, perhaps not a great compromise. But now any piece of code can request a Monoid, and by doing so will automatically be able to fold it down, without really needing any deeper understanding of the datatype itself.