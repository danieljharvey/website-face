---
title: Typeclasses - Alternative
tags: haskell, typeclasses
---

Let's think about things that may or may not happen (again, I know, I'm sorry).

```haskell
data Perhaps a = Yeah a | Nope deriving (Eq)
```

A classic datatype, that we can use for expressing either *Some Sort Of Value* or *The Entire Concept Of Failure*. We can use it when getting the first item of a list, which may or may not actually exist.

```haskell
first :: [a] -> Perhaps a
first (a:_) = Yeah a
first _     = Nope
```

If there IS a first item in the list we get the item wrapped in a `Yeah`, if not we get `Nope`.

We can make another very similar function for getting the second item as well...

```haskell
second :: [a] -> Perhaps a
second (_:b:_) = Yeah b
second _       = Nope
```

Now let's say that for some entirely incomprehensible reason, we'd like the second item in the list, if not, the first item. What would that function look like?

### First solution

```haskell
naiveImplementation :: [a] -> Perhaps a
naiveImplementation as = case second as of
  Yeah s -> Yeah s
  Nope   -> first as
```

Does it work?

```haskell
emptyList :: Perhaps Int
emptyList = naiveImplementation []
-- emptyList == Nope

oneItemList :: Perhaps Int
oneItemList = naiveImplementation [1]
-- oneItemList == Yeah 1

twoItemList :: Perhaps Int
twoItemList = naiveImplementation [1,2]
-- oneItemList == Yeah 2
```

It bloody does! Lovely stuff. Let's all go home and put our feet up, we've nailed it.

OK. But hang on, that `naiveImplementation` function does seem a bit much though, especially if we start adding more cases to it. Plus that name kinda suggests, well, that the function might not represent the best way to go about this.

What if there was a typeclass that was designed to make things like this easier to deal with?

Let's meet `Alternative`!

### Definition

What does `ghci` have to say about this so-called `Alternative`?

```haskell
Prelude> import Control.Applicative
Prelude> :i Alternative
```

```haskell
class Applicative f => Alternative (f :: * -> *) where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  {-# MINIMAL empty, (<|>) #-}
```

OK. So the most interesting thing is that any `Alternative` first needs to be a valid instance of [Applicative](/posts/2018-11-17-typeclasses-applicative.markdown) first.

Secondly, we can get one just by defining `empty` and `<|>`. What are those then?

1. `empty` is the *identity* element. This is a bit like the `mempty` in [Monoid](/posts/2019-01-04-typeclasses-monoid.markdown) - it is a value that when added to the datatype, does nothing to it.

2. `<|>` is sort of an `or` for data types. Given two values, it returns the first *valid* one, as such. What *valid* means depends on the datatype, but usually that value won't be the same as `empty`.

If you're looking at this and thinking "This Seems Very Similar To A Monoid" then, yes, it sort of does, consisting as it does of one *Smash Things Together* function and one *Sort Of Nothingy* value.

Let's define it for our exciting `Perhaps` type.

### Instances

Before we can have an `Applicative` we'll need a [Functor](/posts/2018-11-16-typeclasses-functor.markdown) instance. This one runs the function over the value if it's there, if not, it does nothing at all.

```haskell
instance Functor Perhaps where
  fmap f (Yeah a) = Yeah (f a)
  fmap _ _        = Nope
```

OK. Great stuff. Let's make an `Applicative` instance next. The `pure` instance just wraps whatever it's passed in a `Yeah`. The `<*>` (or `apply`) function is written so that if we have a function inside `f` and a value in `a` then we'll get the function `f` applied to `a` and wrapped in `Yeah`, if not we'll get a `Nope`.

```haskell
instance Applicative Perhaps where
  pure a = Yeah a
  (Yeah f) <*> (Yeah a) = Yeah (f a)
  _        <*> _        = Nope
```

Yeah? Yeah.

OK. Here is the exciting part:

```haskell
instance Alternative Perhaps where
    empty             = Nope
    Yeah x  <|> _     = Yeah x
    _       <|> y     = y
```

Look at that! An exciting `Alternative` instance. `Nope` is our `empty` (or *identity*) value, and our `<|>` function returns the first value if it's a `Yeah` (which represents *valid* data in the context of our `Perhaps` datatype), if not it returns whatever the second value is.

Let's use it to make some contrived code ever so slightly smaller and more difficult to understand.

### Better Solution

Here is our *Get The Second Item, Or If That Doesn't Work Out Then Let's Settle For The First Item* function.

```haskell
getPreferred :: [a] -> Perhaps a
getPreferred as = second as <|> first as
```

Much easier to understand! It almost looks like `second || first`, which I like a lot.

Don't trust me?

```haskell
notFound :: Perhaps Int
notFound = getPreferred []
-- notFound == Nope

found1 :: Perhaps Int
found1 = getPreferred [1]
-- found1 == Yeah 1

found2 :: Perhaps Int
found2 = getPreferred [1,2]
-- found2 = Yeah 2
```

See! And you doubted me!

Now, that seems like a lot of instance writing for not much, but often the place you'll find `Alternative` is in stuff like routing for front end applications. Let's have a look at something like that:

### Routing Example

Here's a datatype to describe all the pages in a completely fictional and somewhat limited website...

```haskell
data Route = Index
           | Gallery
           | Contact
           | Complaints
           | Help deriving (Eq, Show)
```

...some type aliases to make the type signatures we're about to write easier to understand...

```haskell
type Url = String
type Match = String
```

...and a (rather basic) function for taking the passed `url` and matching it to the page we want to be looking at:

```haskell
matches :: Match -> Route -> Url -> Perhaps Route
matches match route url = if isInfixOf match' url'
                        then Yeah route
                        else Nope where
                          match' = toLower <$> match
                          url'   = toLower <$> url
```

What's going on here?

1. First, it converts our `match` and `url` strings to lowercase by mapping `toLower` from `Data.Char` over.

2. Then, it uses `isInfixOf` from `Data.List` which returns a `Boolean` telling us whether the `match` string can be found inside the `url` string.

3. If so, return the passed `Route` wrapped in a `Yeah`, if not, return `Nope`.

Good stuff!

And now here is our actual logic, that uses the `matches` function to find out where we should navigate to in our excellent website.

```haskell
matchRoute :: Url -> Perhaps Route
matchRoute url = matches "gallery" Gallery url
             <|> matches "contact" Contact url
             <|> matches "complaints" Complaints url
             <|> matches "help" Help url
```

Assuming we always want to navigate somewhere, we can also make a wrapper function that returns a default `Route` if we do not find a valid one.

```haskell
matchRouteDefault :: Url -> Route
matchRouteDefault url =
  case matchRoute url of
    Yeah route -> route
    _          -> Index
```

Let's use it to navigate to the `Gallery`, which I have no doubt is a very good example of the genre.

```haskell
findGallery :: Route
findGallery = matchRouteDefault "http://internet.com/gallery"
-- findGallery == Gallery
```

Or default to the `Index` when we're passed a load of old rubbish.

```haskell
findDefault :: Route
findDefault = matchRouteDefault "http://internet.com/rubbish"
-- findDefault == Index
```

We're basically front end development experts now. Great stuff.

### Wrapping Up

The `Control.Applicative` documentation describes `Alternative` as a *"a Monoid on applicative functors"*, and perhaps I could have just led with that and saved us all a bunch of trouble. This kind of typeclass mainly gives us a nicer syntax so that we can bend our code to closer match the domain we are working in, which is generally a Good Thing in my book.

(You may notice we've done a lot of work here defining `Perhaps` when we could have just used `Maybe` and saved ourselves writing a lot of instances but it's my blog and I can do what I want.)

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Control.Applicative](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html)

[Alternative and MonadPlus](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus)
