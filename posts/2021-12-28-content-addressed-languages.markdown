---
title: What the hell is a content addressed language 
tags: plt, mimsa, testing, unison
---

Over the last year and a half I haven't really written much stuff, because I
have been working on a small content-addressed programming language called
[mimsa](https://github.com/danieljharvey/mimsa). I'm not really sure why I am doing this, other than that I watched a
[neat tutorial about how typecheckers worked](https://www.youtube.com/watch?v=frM7GhBERAs), and then got somewhat carried away.

I has sat down to write about my recent implementation of property testing in said
lang, then realised that before I can do that I should probably provide some 
context.

### So, what IS a content-addressed language?

According to [Unison](https://www.unisonweb.org/), the language that pioneered
this concept, this means "code is immutable and identified by its content".

Let's take that apart a bit:

Say I have a function for adding two numbers:

```haskell
add :: Int -> Int -> Int
add a b = a + b
```

Instead of just dumping this in a text file, I would "save" this function. In
the `mimsa` repl, I would do this:

```haskell
:> :bind add = \a -> \b -> a + b
Bound add.
```

This has saved the expression `\a -> \b -> a + b` in the project, and pointed `add`
to refer to said expression.

Let's use our new function:

```haskell
:> add 1 3
4 :: Int

:> add 2 2
4 :: Int
```

Hopefully, no surprises there.

Let's make a new function that uses `add`:

```haskell
:> bind add3 = add 3
```

This has saved the expression `add 3` in the DB, and pointed `add3` at it. More
importantly, it has also remembered exactly which `add` function it used (more
on this shortly).

> Like `haskell` or `elm`, `mimsa` functions only take one argument, so
> applying `3` to `add` (which has type `Int -> Int -> Int`) returns a new
> function with type `Int -> Int`.

Again, let's test it and check for surprises:

```haskell
:> add3 2
5 :: Int

:> add3 0
3 :: Int
```

All seems well.

Now what if we decide to make `add` evil? We are allowed to rebind it, so let's
do that:

```haskell
:> bind add = \a -> \b -> a + b + 1
Updated binding of add.
```

Now when we use `add` we'll get wonky answers as expected.

```haskell
:> add 2 2
5 :: Int

:> add 0 0
1 :: Int
```

However, look what happens when we use `add3`:

```haskell
:> add3 1
4 :: Int
```

We still get the original correct result.

### So, what has happened here?

The answer is in how `mimsa` expressions are stored. When we stored `add`, we
stored not only the expression `\a -> \b -> a + b` , but the fact that
`add` has no dependencies. Internally, it would look a bit like this:

```haskell
-- | Distinct type for our hashes, a wrapper around Text
newtype ExpressionHash
  = ExpressionHash Text

-- | Simplified version of our StoreExpression type
data StoreExpression = 
  StoreExpression {
    -- | the code for this expression
    expression :: Text,
    -- | a map from the names of dependencies we've used to their hashes
    dependencies :: Map Name ExpressionHash
  }

-- how our `add` expression is stored internally
addExpression :: StoreExpression
addExpression = 
  StoreExpression
    { expression = "\\a -> \\b -> a + b",
      dependencies = mempty
    }
```

Once we've made the `addExpression`, we can create a hash of it, which
we'll pretend looks like `"abc123"` in hexadecimal. Then when we `bind` the 
name `add` to it, we can store it in the project:

```haskell
-- | Simplified version of our Project type
data Project
  = Project 
      { -- | map from names like `add` to hashes
        bindings :: Map Name ExpressionHash,
        -- | map from hashes to expressions
        store :: Map ExpressionHash StoreExpression
      }

projectWithAdd :: Project
projectWithAdd = Project
  { bindings: fromList [("add", "abc123")],
    store: fromList [("abc123", addExpression)]
  }
```

Now when we create `add3`, the following happens:

```haskell
add3Expression :: StoreExpression
add3Expression =
  StoreExpression
    { expression = "add 3",
      dependencies = fromList [("add", "abc123")]
    }
```

When evaluating `add 3`, `mimsa` has looked in the project for something called
`add`, found it, and then saved that it is needed in this expression. Because
we refer to `add` by a hash of it's content, this is what makes it "content
addressed".

Assuming `add3` has a hash of `"def456"`, our project would look like this now:

```haskell
projectWithAddAndAdd3 :: Project
projectWithAddAndAdd3 = Project
  { bindings: fromList [
      ("add", "abc123"), 
      ("add3", "def456")
    ],
    store: fromList [
      ("abc123", addExpression),
      ("def456", add3Expression)
    ] 
  }
```

Now, when we come to update `add` and make it evil, we create the following:

```haskell
evilAddExpression :: StoreExpression
evilAddExpression = 
  StoreExpression
    { expression =  "\\a -> \\b -> a + b + 1",
      dependencies = mempty
    }
```

Assuming it has a hash of `"ghi789"`, then the project looks like this:

```haskell
projectWithAddAndAdd3 :: Project
projectWithAddAndAdd3 = Project
  { bindings: fromList [
      ("add", "ghi789"), 
      ("add3", "def456")
    ],
    store: fromList [
      ("abc123", addExpression),
      ("def456", add3Expression),
      ("ghi789", evilAddExpression)
    ] 
  }
```

The project has been updated, but note we haven't deleted any items from the
`store`. All that has been updated in the project is the binding for `add`.
This means any new uses of `add` will use the new broken
function, but `add3` is completely unaffected. If you did want `add3` to adopt
the new terrible behaviour, it would be a case of binding it again, where it
would use the broken `add` function from the project.

### Why would you do this?

#### Maximum cachability

Because any of the expressions we can created don't change once created, they
don't need reparsing or typechecking (or transpiling to JS, etc) over and over. `add` will have the type
`Int -> Int -> Int` forever, meaning that the idea of a slow build is kinda
removed.

#### No namespacing issues

In many languages conflicting names of packages or imports can cause issues. We
might want to use function A from package version 1.1 but function B from
package version 1.3. Most package managers won't let you do this (or they let
it sort of happens sometimes, but weird stuff happens with globals interacting, I'm looking at you React). Expressions 
in a content-addressed language refer to each other by hashes rather than names, 
so function A and function B will have no idea about one another unless they
depend on one another somehow, and everything works great.

#### Granular code sharing

Because of the above, the idea of the package as the unit of shared
functionality is somewhat obselete. [Unison Share](https://share.unison-lang.org/)
demonstrates what it could be like if we shared smaller units instead. 

#### Tests don't need running over and over

If we test that `add 1 3 == 4`, and I know that `add` is not going to change,
then I can keep this test result around and don't need to run it again. When
`add` is rebound, we can make a copy of the test and run it on the new
implementation to see if the same test still passes. Property tests, it turns
out, aren't quite so simple, but I'll come to that in a later post.

### Doesn't this mean that most developer tools that are built assuming programming means text files and diffs to said files are all unusable here?

Ahem.

### That's all the words

That's some context of what content-addressed languages are, at least, my weird
understanding of them anyway. The actual implementation in `mimsa` is more
complicated than above (for instance, we store the raw AST rather than the
text syntax so we can change the syntax without breaking the expressions, and
store the history of name bindings rather than just the newest one) but
hopefully it gives you a clue about what is going on under the hood.

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[Unison - the big idea](https://www.unisonweb.org/docs/tour)

[mimsa](https://github.com/danieljharvey/mimsa)
