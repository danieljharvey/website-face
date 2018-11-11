---
title: Writer Monad
---

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
