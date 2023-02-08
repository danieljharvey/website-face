---
title: Compiling a language to LLVM, part 1 
tags: plt, mimsa, typechecker, llvm 
---

Recently I thought it would be good to start compiling the small functional
language [mimsa](https://github.com/danieljharvey/mimsa) I've been messing
around with for the last few years to LLVM. There's a few really helpful
resources - Joseph Morag's [Micro-C](https://blog.josephmorag.com/posts/mcc0/)
series, and [Mapping High Level Concepts to LLVM
IR](https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/) in
particular, but neither go into converting functional programming paradigms
such as pattern matching, lambdas and ADTs in much depth.

So, here we're going to give it a go. The plan is that we start with an
entirely working but tiny language, get it all working, and then as we add
features to the language, we'll also add a typechecker and introduce more LLVM
concepts.

## Our language

The first iteration of our language is going to be the most basic of
calculators. Here is a valid program in our language:

```bash
1 + 2 * (1 + 100) - 100
```

Here are the datatypes for our language. We will use this to represent our
programs internally.

```haskell
data Expr ann
  = -- | An integer value
    EPrim ann Int
  | -- | Two values combined with an operator
    EInfix ann Op (Expr ann) (Expr ann)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Operators for combining integers
data Op = OpAdd | OpMultiply | OpSubtract
  deriving stock (Eq, Ord, Show)
```

Here's some example values:

```haskell
-- | the number `1`
one :: Expr ()
one = EPrim () 1

-- | the expression `2 + 3`
twoPlusTwo :: Expr ()
twoPlusTwo = EInfix () OpAdd (EPrim () 2) (EPrim () 3)
```

We call `Expr` an Abstract Syntax Tree (AST).

You may notice the `ann` type variable. We'll use this to "decorate" items in
our AST. For now, we'll just use `()`.

## Creating AST terms

Although we could provide our compiler as a Haskell library and ask our users
to manually create `Expr` values for us, it'd be much nicer to be able to read
input from a file.

This process is called _parsing_, and looks something like this:

```haskell
parseInput :: Text -> Either ParseError (Expr ())
```

Given some user input, parsing returns either a valid `Expr` or a (hopefully)
helpful error.

We'll be using a library called `megaparsec`.

### Lexemes

"1+1" and "  1    + 1" should mean the same thing. To save us having to eat
through all the spaces we'll use a `lexeme`. This is a sort of rule that says
"when eating this value, also eat all the whitespace before it". We'll wrap all
our parsers in `myLexeme`, which will give it whitespace-eating powers.

### Parsing infixes

One problem with parser combinator libraries is that it's easy to get into an
infinite loop. `megaparsec` has a bunch of useful helpers for this, in
particular, support for `Operator`. This lets us do stuff like "make `*` more
important than `+` and `-`".

```haskell
-- | to make infix stuff like easier, `megaparsec` provides
-- the `Operator` type
table :: [[Operator Parser (Expr Annotation)]]
table =
  [ [binary "*" (EInfix mempty OpMultiply)],
    [ binary "+" (EInfix mempty OpAdd),
      binary "-" (EInfix mempty OpSubtract)
    ]
  ]

binary :: Text -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ stringLiteral name)
```

### Source code location

You may have noticed the `ann` type variable in our `Expr` type. This allows us
to "decorate" our AST nodes. In parsing, it's useful to decorate each node with
it's location in the code.

We use the following datatype for this:

```haskell
data Annotation = Location Int Int
  deriving stock (Eq, Ord, Show)
```

The two `Int` values are "character number we start at" and "length of the
string". Therefore parsing " 100" should return:

```haskell
parseResult :: Expr Annotation
parseResult = EPrim (Location 1 3) 100
```

We'll worry about breaking it up by line later.

## Typechecking

So now we've gotten a nicely parsed AST, it's typechecking time! However, our
language has been carefully chosen to make constructing nonsense values
impossble. Therefore, we'll leave it for today, but worry not, we'll soon need
to address this!

## A simple interpreter

Before we get stuck into `LLVM`, it's good to be able to evaluate our language
internally. Doing so isn't too complicated though:

```haskell
interpret ::
  Expr ann ->
  Expr ann
interpret (EPrim ann p) = 
  -- don't do anything with these
  EPrim ann p
interpret (EInfix ann op a b) =
  -- attempt to simplify these 
  interpretInfix ann op a b

interpretInfix ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  Expr ann
interpretInfix ann OpAdd (EPrim _ a) (EPrim _ b) =
  -- add the int values
  EPrim ann (a + b)
interpretInfix ann OpSubtract (EPrim _ a) (EPrim _ b) =
  -- subtract the int values
  EPrim ann (a - b)
interpretInfix ann OpMultiply (EPrim _ a) (EPrim _ b) =
  -- multiply the int values
  EPrim ann (a * b)
interpretInfix ann op a b = 
  -- simplify the leaves and try again
  interpretInfix ann op (interpret a) (interpret b)
```

`EPrim` values don't really do anything.

When we see an `EInfix` value, we look for `EPrim` values inside and `add` / `subtract` / `multiply` them. If they're
not `EPrim` values yet, then they must be nested expressions, so we `interpret` them
and try again.

Note that even though we'll probably be interpreting a `Expr Annotation`, we'll
talk about `Expr ann`, as we'll be ignoring the annotations.

## Let's use LLVM

### What is LLVM

It's a compiler backend, part of `clang`. It's very C shaped, and used by
`Rust` and `Swift` amongst others. It's got a bunch
of number types, structs, and arrays, functions, and enough pointers to have a
very bad time. Fortunately for now we won't need too much. 

### Making a module

We'll be creating LLVM IR (Intermediate Representation)

It's a language that looks like hell:

Here is the representation for printing the number `42` to `stdout`.

```llvm
; ModuleID = 'calc'

declare external ccc i32 @printint(i32)

define external ccc i32 @main()    {
  %1 =  call ccc i32 @printint(i32  42)
  ret i32 0
}
```

Fortunately, we won't need to write this by hand.

- imports
- externs (native fns)
- functions (like main)
- stdlib (in C)

### Compiling and running it

We write our module to "module.ll" using `llvm-hs-pretty`, then run `clang` to
put everything together.

```bash
clang -Wno-override-module -lm module.ll runtime.c -o a.out 
```

This should output an executable called `a.out`

### Our "runtime"

We skipped something above - what is `runtime.c`?

A program language runtime is a bunch of helper code written in the target
language. Ours has a single function `printint` that takes an `int` and prints
it to `stdout`.


```c
#include <stdio.h>
#include <stdlib.h>

void printint(int i) {
  printf("%d", i);
}
```


Use the command line, fuck it.

### summing up 

write some stuff

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[mimsa](https://github.com/danieljharvey/mimsa)
