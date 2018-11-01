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
foldr f def [] = def
foldr f def (x : xs) = x `f` foldr f def xs
```

It actually takes three arguments:

1. A function with the type `a -> b -> b`.
2. The starting value for the fold of type `b`.
3. A list of values we wish to add together of type `[a]` - in our example this could be `["hot", "dog", "frog"]`.

If you squint, you're pretty much looking at the `array.reduce` function in Javascript.

(What is important to note when looking at signatures for functions like this is that `a` and `b` may happen to be the same type - the important thing here is that they don't have to be. When using a list of strings like we are, the type is actually `(string -> string -> string) -> string -> [string] -> string` which is simultaneously more and less confusing.)

Let's trying using it with our string `concat` function from earlier.

```haskell
output = foldr concat "" ["hot","dog","frog"]

-- output = "hotdogfrog"
```

Great! Hopefully this all makes sense - we passed `foldr` a function for combining strings and a starting value of an empty string. Why the starting value? Well, what would happen if we folded over an empty list?

```haskell
output2 = foldr concat "" []

-- output2 = ""
```

Great! We get our default value rather than some sort of awful error. Could we use just anything though? It would be kind of nice to have a sort of error value as a default, I suppose:

```haskell
output2 = foldr concat "sorry, no value" []

-- output2 = "sorry, no value"
```

Seems nice, however this wouldn't work when we do have values:

```haskell
output = foldr concat "sorry, no value" ["hot","dog","frog"]

-- output = "sorry, no valuehotdogfrog"
```

I guess we need empty values that don't mess up the result then. Let's stick to `""` for strings for now.

What's Monoid then?

#### A Monoid is a way of generalising over the idea of combining things

It needs two things:

1. An operation for combining two values (called `mappend`)
2. An empty value to start from (called `mempty`)

For strings the combining operation is our `concat` and our empty value is `""`. The key observation here is that adding `""` to the start or end of a string changes nothing about the string.

Our example actually has another example within it - the list itself! Here is the Monoid instance for a list:

```haskell
mappend :: [a] -> [a] -> [a]
mempty :: []
```

Using `concat` on two lists will smash one after the other, ie `concat [1,2,3,4] [5,6,7]` would make `[1,2,3,4,5,6,7]`. However, `concat`-ing / `mappend`-ing an empty list will do nothing, just like adding an `""` to the end of a string.

The rule then:

#### If you concat the empty element onto a Monoid, the Monoid remains the same

Anyway, that's quite enough for now I feel. I have some more but will keep it for the next thing.

Further reading:

[Data.Monoid on Hackage](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html)

[5 minutes to Monoids](https://medium.com/@sjsyrek/five-minutes-to-monoid-fe6f364d0bba)
