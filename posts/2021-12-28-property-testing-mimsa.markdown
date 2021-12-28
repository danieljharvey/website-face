---
title: Property testing a content-addressed language 
tags: plt, mimsa, testing 
---

So in the [last post](/posts/2021-12-28-content-addressed-languages.html) I quickly introduced the project I've been working on for a
while called [mimsa](https://github.com/danieljharvey/mimsa). The reason I did this was so that I could write this post
about recently adding property tests to the language, and what was good/bad
about that, so without further ado here are many words: 

### Tests in mimsa

So, for context, unit tests work as follows in `mimsa`:

- We create a (broken) `add` function:

```haskell
:> :bind add = \a -> \b -> a + b + 1
Bound add.
```

- We create a unit tests that confirms that `add` is indeed broken:

```haskell
:> :addTest "2 + 2 == 4" add 2 2 == 4
--- FAIL --- 2 + 2 == 4
```

- We update `add` to a working implementation, and now the test passes:

```haskell
:> :bind add = \a -> \b -> a + b
Updated binding of add.
+++ PASS +++ 2 + 2 == 4 
```

What is really neat is that what looks like __one test being run twice__ is
actually __two tests being run once each__.

Why? The reason is immutability - once the first test is
created it can't be changed. It also doesn't need re-running - it always has
the same arguments, and it's only dependency, the original `add` function,
never changes, so the test stands as a static assertion of the behaviour of
said function.

And where does the second test come from? When we bind a new version of `add`, it stands to reason we might
want to run the same tests on it, therefore a new copy of any test is created
that uses the new `add` function as it's dependency.

> A really nice property of testing in this way is that we could also make it
> run in reverse. If I add another test to the new `add` function there is no
> reason not to auto-create versions of it for the old `add` bindings too. This 
> is super helpful when finding a new regression, as we can write a new test for 
> old code to find out when said regression occurred, a sort of super-powered 
> git bisect. I say _could_ here because I have not done this yet because I am
> lazy.

### Property testing, a recap

Property testing is a kind of testing where instead of confirming our code
works against known good values we confirms that it obeys certain rules.

A non-property test could be `add 2 2 == 4` - we know this should be the answer
because we have checked on a calculator that `2 + 2` does indeed equal `4`.

A property test might be `add 0 a == a`, given any `a`. When this is run by
[quickcheck](https://hackage.haskell.org/package/QuickCheck) or
[fast-check](https://github.com/dubzzz/fast-check) the property testing
framework will run the test with loads and loads of different values for `a`,
and return all the ones that break it (in this case, none of them).

Property testing is very useful for finding edge cases in functions, and used
well, can be a lot more thorough than unit tests, for not a lot more effort.

### The current implementation

For a unit test to be valid in `mimsa`, it must have the type `Boolean` (There
is a lot wrong with this, as it doesn't let you see what went wrong, but we
can't just go around making things "good" all the time).

Therefore this are the simplest possible tests: 

```haskell
:> :addTest "true" True
+++ PASS +++ true

:> :addTest "false" False
--- FAIL --- false
```

To make a property test, we provide an expression with the type `something -> Boolean`, like:

```haskell
:> :addTest "or true is always true" \bool -> or bool True
+++ PASS +++ or true is always true
```

What's happened here?

The `mimsa` typechecker has inferred that `bool` must be a `Boolean`, generated
a big bucket of booleans, and then checked that each time `or bool True ==
True`. This returns `True` everytime so the test passes.

> The `or` function has type `Boolean -> Boolean ->
Boolean` and looks something like `\a -> \b -> if a then True else b`. It is
similar to the `||` operator in most programming languages.


Let's try another:

```haskell
:> :addTest "and true is always true" \bool -> and bool True
--- FAIL --- and true is always true
Failing inputs:
 - False
```

This property test has failed, and it's telling us that when you pass `False`
to the test function, it does not return `True`. The `and` function here is
equivalent to `&&`, so this makes sense, as `False && True` equals `False`.

As the input is a `Boolean` there are only two inputs, and the `mimsa` property
test generates `1000` sample inputs, it's fairly likely we'll get both a `False`
and a `True` input. 

### Breaking it

However, it's not too difficult to make a less definitive test:

```haskell
:> :bind flakyTest = 
  \val -> match val with 
    (Just (Just (Just (Just True)))) -> False 
  | _ -> True
Bound flakyTest.

:> :addTest "Flaky" flakyTest
+++ PASS +++ Flaky

:> :addTest "Flaky 2" flakyTest
--- FAIL --- Flaky 2
Failing inputs:
 - Just (Just (Just (Just True)))
```

Because the input type here would be `Maybe (Maybe (Maybe (Maybe Boolean)))` there are a lot of potential inputs,
and it's quite possible that `Just (Just (Just (Just True)))` is not amongst
the generated values.

Another type of flaky test can be generated like this:

```haskell
:> :addTest "flaky string" \str -> not (str == "dog")
+++ PASS +++ flaky string
```

It passes every time I have tried it, although as a statement it makes no
sense - _there is no string that equals "dog"_. This kind of test is hard just
because there are so many potential strings out there that a total search of
the space is close to impossible.

### The problems / solutions

#### Simple tests could be non-deterministic and aren't:

```haskell
\bool -> and bool True
```

This could be two unit tests that are run once and never again, however as a
property test it must be run over and over. This is the least of the problems
but worth mentioning.

#### Complex finite tests become flaky

```haskell
\val -> match val with
    (Just (Just (Just (Just True)))) -> False
  | _ -> True
```

Given that a `Maybe` can of one two things, as can a `Boolean`, 
there are `2 x 2 x 2 x 2 x 2 == 32` potential inputs here. If we generated 
the cartisian product of all inputs methodically then we could cover all inputs
and run the tests against them all. This would ensure a) the same results each
time and b) that we could cache the results once and never run them again.

However, if the input type contains an `Int`, `String`, `Array` or recursive type 
(such as `type List a = Nil | Cons a (List a)`) then generating a complete set
of inputs is no longer practical. 

#### String and number spaces just too huge

```haskell
\str -> not (str == "dog")
```

Due to the way the `mimsa` web client works, we can't really fall back to brute
forcing an absolute shit ton of inputs for `String` and `Int` inputs. However, 
we have access to the AST when running the test. Would it be so terrible to add all of the string and number literals found in the
expression into the test values? For instance, if we used the input `"dog"`
above we'd find the breaking case immediately.

### No more words

That is the end of the words. It's a bit of a braindump, and pretty much only
for my own benefit, but I hope it is perhaps vaguely interesting. Who knows?

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[quickcheck](https://hackage.haskell.org/package/QuickCheck)

[fast-check](https://github.com/dubzzz/fast-check)

[mimsa](https://github.com/danieljharvey/mimsa)
