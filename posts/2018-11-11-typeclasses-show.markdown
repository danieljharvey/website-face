---
title: Typeclasses - Show
tags: haskell, typeclasses
---

Let's think about things.

```haskell
data Thing = Thing1 | Thing2
```

Seems great so far. Let's try and look at one in `ghci`.

```haskell
Prelude> show Thing1
```

Oh shit!

```haskell
• No instance for (Show Thing) arising from a use of ‘show’
• In the expression: show Thing1
    In an equation for ‘it’: it = show Thing1
```

Totally bogus!

So what's the problem? So for something to be shown in the console, we need to know how to convert it into a String. This is a pretty common thing to be able to do, so we have a typeclass for it, called `Show`. Let's check it out in `ghci`.

```haskell
Prelude> :i Show
```

```haskell
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
```

A few more things than expected, tbh. Fortunately, that `{-# MINIMAL showsPrec | show #-}` means we can just define `show`, which takes any value of a type and turns it into a `String`.

Let's make another data type.

```haskell
data BetterThing = Better1 | Better2
```

This time, we'll create an instance of `Show` for it.

```haskell
instance Show BetterThing where
    show Better1 = "Better thing 1"
    show Better2 = "The other even better thing"
```

Now we can `show` it as much as we like.

```haskell
showBetter1 :: String
showBetter1 = show Better1
-- showBetter1 = "Better thing 1"
```

```haskell
showBetter2 :: String
showBetter2 = show Better2
-- showBetter2 = "The other even better thing"
```

Great! What if we don't need particularly clever string output? Just the name of the type would be fine. Can we cheat and derive one?

```haskell
data LazyThing = Lazy | Crazy | Other deriving (Show)
```

Nothing seemed to complain there. Good stuff!

```haskell
showLazy :: String
showLazy = show Lazy
-- showLazy == "Lazy"
```

Success! And, so, as one would expect...

```haskell
showCrazy :: String
showCrazy = show Crazy
-- showCrazy == "Crazy"
```

That's better, but is there anything a bit more interesting we could do with this? Sure thing! What about a data type for creating HTML markup?

```haskell
type Href = String

data HTMLTree = Div [HTMLTree a]
              | Title String
              | P String
              | A Href [HTMLTree a]
```

That `type` alias for `Href` just means it's a normal string, but it makes the intent a bit clearer. So this little markup type has four kinds of element.

1. `Div` has no content of it's own but is also a wrapper for a list of `HTMLTree a` elements.
2. `Title` also takes a `String` and has no child elements.
3. `P` takes a `String` and has no child elements.
4. `A` takes an `Href` (an alias for `String`) and a list of children of type `HTMLTree a`.

Let's put it to work making an internet website.

```haskell
website :: Tree String
website = Div
        [ Title "My interesting website"
        , P "Item of interest"
        , A "http://danieljharvey.github.io" [ P "Home" ]
        , Div [ P "Sub-item"
              , P "Another sub-item"
              ]
        ]
```

Looks like the beginnings of a world beating internet business to me. How can `Show` help us though?

```haskell
instance (Show a) => Show (Tree a) where
    show (Div as) = "<div>" ++ concatSubtags as ++ "</div>"
    show (A href as) = "<a href='" ++ href ++ "'>"
                     ++ concatSubtags as ++ "</a>"
    show (P a) = "<p>" ++ a ++ "</p>"
    show (Title a) = "<h1>" ++ a ++ "</h1>"

-- utility function for showing child items
concatSubtags :: (Show a) => [Tree a] -> String
concatSubtags as = concat $ fmap show as
```

By defining a `show` function for each of our tags (and recursively calling `show` for tags with child elements) we can generate HTML markup using one easy function:

```haskell
Prelude> show website
```

Hooray! A website!

```html
<div>
  <h1>My interesting website</h1>
  <p>Item of interest</p>
  <a href="http://danieljharvey.github.io"> <p>Website</p> </a>
  <div>
    <p>Sub-item</p>
    <p>Another sub-item</p>
  </div>
</div>
```

(Actually it comes out in one big line, and then I added the line breaks to fit it on the page. Oh, the lies!)

It could do with a bit of sprucing up, and a few more tags, but in my by no means humble opinion we're on our way to creating the next React-beater. Great job, `show`.

(Incidentally, although this is a more fun example of `show`, I'd probably recommend just using a normal function for this kind of thing. Something classic like `renderTree :: (Show a) => Tree a -> String` maybe. This isn't strictly what the `Show` typeclass is for. There is also another type class called `Read` whose job is to turn the `String` output of `show` back into the original data type, and so making a clever `show` instance you're somewhat implicitly promising that you can turn it back somehow. This could end up being a Bad Time, and that's exactly the kind of time we don't want to have.)

Make sense? If not, why not [get in touch](/contact.html)?

Further reading:

[Text.Show](http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Show.html)
