---
title: Typeclasses - Traversable
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

We could add them up with something like this:

```haskell
maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd (Just a) (Just b) = Just (a + b)
maybeAdd _ _ = Nothing

maybeTreeTotal :: Maybe Int
maybeTreeTotal = foldr maybeAdd (Just 0) maybeTree
-- maybeTreeTotal == Just 12
```

But what if we want to remove all the `Just` and `Nothing` from the `MyTree` altogether? Enter `Traversal`!

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

1. The `(Functor t, Foldable t) =>` part means we'll need to create `Foldable` and `Functor` instances first.
2. The `Applicative f =>` part means whichever other type we use it with must have an instance of `Applicative`. Fortunately `Maybe` does.
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

It's perhaps not the most intuitive thing to look at and understand, so let's try using and see what's up. Although we've implemented the typeclass using `traverse`, the more intuitive function it provides us is `sequence`, which is sort of a "swap the types around" function.

```haskell
justTree :: Maybe (MyTree Int)
justTree = sequence maybeTree
-- justTree == Just (Branch (Branch (Leaf 2) (Leaf 3)) (Branch (Leaf 5) (Leaf 2)))
```

All of the `Maybe` values in `maybeTree` were `Just`, so this gives us a `Maybe`-less `MyTree Int` wrapped up inside a single `Maybe`. If this seems a bit odd to comprehend, it might remind you of this javascript:

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
anotherMaybeTree :: MyTree (Maybe Int)
anotherMaybeTree = Branch (Leaf Nothing) (Leaf $ Just 3))
```

```haskell
nothingTree :: Maybe (MyTree Int)
nothingTree = sequence anotherMaybeTree
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
