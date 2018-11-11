---
title: More Monoids
---

Previously we discussed Monoids, which are basically a way to make things that you do understand, like strings and lists, into somebody you don't understand again. Thanks Monoids. Thonoids.

To further muddy the water, here are some more monoids that should make things even less clear.

Monoids don't need to be lists or arrays of things either, they can also apply to the behaviour of numbers. Integers, for instance, can form several Monoids. One is addition:

```haskell
newtype MySum a = MySum {
    getMySum :: a
}

instance (Num a) => Semigroup (MySum a) where
    MySum a <> MySum b = MySum (a + b)

instance (Num a) => Monoid (MySum a) where
    mempty = MySum 0
```

Here, we have created a `newtype`, which is a wrapper around a value giving a new type, that allows us to treat it differently. We create a value with the `MySum` constructor, or we run `getMySum` on the value to get it out again. How might we use this?

```haskell
ten :: Int
ten = getMySum $ MySum 1 <> MySum 7 <> MySum 2
-- ten == 10
```

Adding `0` onto a number achieves nothing whatsoever, hence it is an ideal empty element.

Another example is multiplication:

```haskell
mappend :: Int -> Int -> Int
mappend a b = a * b

mempty :: Int
mempty = 1
```

Multiplying any number by zero would return zero, and that's certainly not 'doing nothing' - therefore here our empty value is `1`.

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

Therefore, we can just combine our list of strings like thus: `fold ["horse","time","great"]` to make `"horsetimegreat"`.

Wonderful!

So. The question on everybody's lips...what is the point in this?

Well, when doing something simple like this, perhaps not much. We have saved typing around ten letters in exchange for making the function of our code less opaque, perhaps not a great compromise. But now any other function could request a Monoid, and by doing so will automatically be able to combine it in some way, without really needing any deeper understanding of the datatype itself. And combining doesn't have to mean just concatenating - it could be combining a bunch of `Maybe`s, collapsing a tree or [many other things](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html). The important thing is that the other function doesn't really need to care how it works, that's `Monoid`'s job.

Anyway, that's quite enough for now I feel.
