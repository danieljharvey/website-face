---
title: Purescript - Effect and Aff
tags: Purescript
---

Purescript is a Haskell-like language that compiles to Javascript. Most Haskell concepts transfer fairly directly between the two languages, however one big change is the difference between Haskell's `IO` and Purescript's `Effect` monads. Both are the starting point for any program, so whilst in Haskell you'll see:

```haskell
main :: IO ()
```

...in Purescript you'll see the following:

```haskell
main :: Effect Unit
```

Seems quite similar, however, whilst `IO` blocks execution, `Effect` doesn't.
