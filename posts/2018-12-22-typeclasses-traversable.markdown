---
title: Typeclasses - Traversable
tags: haskell, typeclasses
---

Let's think about Trees.

```haskell
data MyTree a = Leaf a | Branch (MyTree a) (MyTree a)
```

A tree can be either a `Leaf` with contains some of value, or a Branch that has two slots for either another `Branch` or perhaps a `Leaf`.

We can build up a tree like this:

```haskell
sampleTree :: MyTree Int
sampleTree = Branch (Branch (Leaf 2) (Leaf 3)) (Branch (Leaf 5) (Leaf 2))
```

So if you remember way back when when we talked about [foldable](/posts/2018-12-03-typeclasses-foldable.html), we can very easily teach this `MyTree` type we have created to fold over itself and do handy things.

```haskell
instance Foldable MyTree where
  foldMap f (Branch l r) = (foldMap f l) <> (foldMap f r)
  foldMap f (Leaf a) = f a
```

Then we can ask it to do helpful things like add up all the values in the tree.

```haskell
sampleTreeTotal :: Int
sampleTreeTotal = getSum $ foldMap Sum sampleTree
-- sampleTreeTotal == 12
```

Great!

This is all very well and good, but what if our tree contains more complex types that just a number? It is entirely plausible to end up with a bunch of optional values like this:

```haskell
maybeTree :: MyTree (Maybe Int)
maybeTree = Branch (Branch (Leaf $ Just 2) (Leaf $ Just 3)) (Branch (Leaf $ Just 5) (Leaf $ Just 2))
```

We could add up all the values without too much trouble, but what if we want to remove all the `Just` and `Nothing` from the `MyTree` but keep the structure intact? Enter `Traversal`!

### Traversable

What does `ghci` have to say about it?

```haskell
*Main> :i Traversable
```

```haskell
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  {-# MINIMAL traverse | sequenceA #-}
```

A few things here:

1. The `(Functor t, Foldable t) =>` part means we'll need to create `Foldable` and `Functor` instances for our datatype before it's allowed to be `Traversable` too.
2. The `Applicative f =>` part means whichever other type we use it with must have an instance of `Applicative`. Fortunately many useful typeclasses are.
2. We can implement either `traverse` or `sequenceA` and the rest will sort itself out.

We'll choose `traverse` for our example.

Firstly, let's make a `Functor` instance for `MyTree`. Nothing untoward here, we just recurse through the tree and run `f` on any `a`s we find laying around. We'll not need to use this directly ourselves but it be used internally by other functions.

```haskell
instance Functor MyTree where
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  fmap f (Leaf a) = Leaf (f a)
```

Now let's implement a `Traversable` instance for `MyTree`.

```haskell
instance Traversable MyTree where
  traverse f (Branch l r) = Branch <$> (traverse f l) <*> (traverse f r)
  traverse f (Leaf a) = Leaf <$> f a
```

A few notes here:

1. `<$>` is the infix version of `fmap`. `(+1) <$> Just 2` is the same as `fmap (+1) Just 2`. Writing it this way shows us how similar it is to the `functor` instance.
2. `<*>` is the infix version of `apply` from `Applicative`. Our `Applicative f =>` constraint means this is supplied by whichever `Applicative` we are using in this function. As we will see, this means different applicatives give us very different outcomes.

It's perhaps not the most intuitive thing to look at and understand, so let's try using and see what's up. Although we've implemented the typeclass using `traverse`, the more intuitive function it provides us is `sequence`, which is sort of a "swap the types around" function. Let's try it with a few different instances of `Applicative` to get a feel for it.

### Maybe

Although we just casually used `Maybe`, earlier, let's clarify what it is. The type definition looks something like this:

```haskell
data Maybe a = Just a | Nothing
```

It is just for holding a value that might be there (`Just "i am a value"`) or expressing a lack of value (`Nothing`). Let's put some in our `MyTree`.

```haskell
maybeTree :: MyTree (Maybe Int)
maybeTree = Branch (Branch (Leaf $ Just 2) (Leaf $ Just 3)) (Branch (Leaf $ Just 5) (Leaf $ Just 2))
```

...and use `sequence` to pull them out and wrap the whole thing in a `Maybe` instead.

```haskell
justTree :: Maybe (MyTree Int)
justTree = sequence maybeTree
-- justTree == Just (Branch (Branch (Leaf 2) (Leaf 3)) (Branch (Leaf 5) (Leaf 2)))
```

What's gone on here then? All of the `Just` values inside the tree have gone, but have been replaced with a single `Just` at the start. If you squint, it kind of looks like we've turned the types inside out, and we kind of have. If this seems a bit odd to comprehend, it might remind you of this javascript:

```javascript
const promise1 = Promise.resolve("yeah")
const promise2 = Promise.resolve("great")

Promise.all([promise1, promise2]).then(as => {
  console.log(as); // ["yeah", "great"]
})
```

Here we can taken an array of Promises, and returned a single Promise returning an array of values. However, what happens if one of those promises fails?

```javascript
const promise1 = Promise.resolve("yeah")
const promise2 = Promise.reject("great")

Promise.all([promise1, promise2]).then(as => {
  // never happens!
}).catch(() => {
  console.log('everything went terribly wrong')
})
```

It short circuits and fails! Going back to our tree, let's try putting a `Nothing` in there and see how that changes things.

```haskell
nothingTree :: Maybe (MyTree Int)
nothingTree = sequence Branch (Leaf Nothing) (Leaf $ Just 3))
-- nothingTree == Nothing
```

Ok! So the same behaviour! Why is that though? There's nothing in our `MyTree` structure that does any checking of these sorts of things.

The key here is in how `apply` (or `<*>`) is implemented in `Maybe` itself:

```haskell
instance Applicative Maybe where
    pure = Just

    Just f  <*> m      = fmap f m
    Nothing <*> _      = Nothing
```

Basically, as soon as we find any `Nothing`, whatever we're doing becomes `Nothing`, so therefore `Maybe` is giving us our same short-circuiting behaviour from the javascript Promise.

Good? Great. Let's look at another.

### List

`List` is another interesting `Applicative` in that it treats every list like a set of possibilities.

It's defined as something like this:

```haskell
data List a = Cons a (List a) | Nil
```

Look what happens when we `sequence` this small tree that contains a `List` in each of it's leaves...

```haskell
invertedListTree :: [MyTree Int]
invertedListTree = sequence Branch (Leaf [1,2]) (Leaf [3,4])
{-
invertedListTree ==
  [ Branch (Leaf 1) (Leaf 3)
  , Branch (Leaf 1) (Leaf 4)
  , Branch (Leaf 2) (Leaf 3)
  , Branch (Leaf 2) (Leaf 4)
  ]
-}
```

It returns a `List` of every possible `MyTree Int` that could be made using the items in each list. If you look at the page on [applicative](/posts/2018-11-17-typeclasses-applicative.markdown) - particularly the `applicativeList` - this may make more sense.

This `sequence` function we are using is merely `traverse id` by the way - so we can start to mess with it even more by using `traverse` with different functions. By passing the `reverse` function, we can get the same thing but backwards...

```haskell
reversedListTree :: [MyTree Int]
reversedListTree = traverse reverse listTree
{-
reversedListTree ==
  [ Branch (Leaf 2) (Leaf 4)
  , Branch (Leaf 2) (Leaf 3)
  , Branch (Leaf 1) (Leaf 4)
  , Branch (Leaf 1) (Leaf 3)
]
-}
```

(Why? I don't know, it's difficult coming up with useful examples all the time, give me a break.)

What about `Either`?

### Either

`Either` is used to express a value that could be one of two things. It's datatype looks something like:

```haskell
data Either a b = Left a | Right b
```

`Left` usually expresses an error or something, whilst `Right` expresses everything being somewhat Hunky Dory.

What happens when all the values are `Right`?

```haskell
rightTree :: Either String (MyTree Int)
rightTree = sequence Branch (Leaf $ Right 100) (Leaf $ Right 200)
-- rightTree == Right (Branch (Leaf 100) (Leaf 200))
```

OK, that seems reasonable, just like the `Maybe` really.

What about if we throw a `Left` in there?

```haskell
failsTree :: Either String (MyTree Int)
failsTree = sequence $ Branch (Leaf $ Right 1) (Leaf $ Left "2")
-- failsTree == Left "2"
```

OK - we get the `Left` value back, which seems reasonable (especially looking at the type signature of our new structure).

What about if there are multiple `Left` values inside?

```haskell
failsTree2 :: Either String (MyTree Int)
failsTree2 = sequence $ Branch (Leaf $ Left "1") (Branch (Leaf $ Left "2") (Leaf $ Left "3"))
-- failsTree2 == Left "1"
```

Oh. That seems somewhat counterintuitive. Even though we have many `Left` items in our tree, when we `sequence` them we only get the first one back. If you want to know why - sure - it's because of the way `Either` implements the `<*>` function. Is there an alterntaive in which we get more of the `Left` items back? Yes - see the bonus item at the bottom of the page.

### So what does this mean?

Basically (lol), what these examples hopefully start to show is that `traverse` let's us combine different types together. Whilst `Foldable` used a `Monoid` instance to combine values together with `<>`, `Traversable` lets us combine them together using their `Applicative` instance and `<*>`. `Applicative` is a pretty powerful typeclass that let's us do a lot of wild shit, so `Traversable` ends up pretty powerful as a result. A lot of the `Lens` package uses `Traversable`, so understanding this gives you a much better idea of what's doing.

There are plenty of `Traversable` instances in the wild to play with, so try smashing a few things together and see what happens. In case it's not entirely obvious, that's basically all I did in writing this article.

Anyhow, that's enough for now I think.

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Data.Traversable](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Traversable.html)

### Bonus item: Validation

If you were enjoying the `Either` example but wondering if there's a way to gather all of the `Left` values in a tree, then, firstly, yes, and secondly, it's done using a different datatype called `Validation`. We won't go into it in depth now, but it's much like an `Either` that let's you collect `Left` items together.

```haskell
validationTree :: MyTree (Validation [String] Int)
validationTree = Branch (Leaf $ Success 100) (Branch (Leaf $ Failure ["2"]) (Leaf $ Failure ["3"]))
```

```haskell
collectFails :: Validation [String] (MyTree Int)
collectFails = traverse id validationTree
-- collectFails == Failure ["2","3"]
```
