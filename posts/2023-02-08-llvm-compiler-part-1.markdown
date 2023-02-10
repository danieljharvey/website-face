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

We'll not go into too much depth as there are already lots of great references
on parsing, but a few details are worth noting.

### Which library?

We'll be using a library called
[megaparsec](https://hackage.haskell.org/package/megaparsec). It's fast, it's got a [great
tutorial](https://markkarpov.com/tutorial/megaparsec.html), and it generates errors that we can render nicely with the [diagnose](https://github.com/Mesabloo/diagnose) library. 

### Dealing with whitespace 

Although some languages make whitespace meaningful, we won't be bothering with
any of that for now. However we would like to allow our users some flexibility
with how they lay out their code. `1+1` and `  1    + 1` should mean the same thing. That means we either need a lot of code like this:

```haskell
parseInfix :: Parser Int
parseInfix = do
  _ <- space0 -- this accepts 0 or more spaces 
  a <- parseInt
  _ <- space0 -- space between number and operator
  op <- parseOp
  <- <- space0 -- space between operator and second number
  b <- parseInt
  pure (Infix op a b)
```

...or we need a neat way of automating this. `megaparsec` has a handy thing
called a `lexeme`. This is a sort of rule that says
"when eating this value, also eat all the whitespace before it". When you see
`myLexeme` in the code, this means we're wrapping a parser with
whitespace-eating powers. This is
covered in much better details in the
[tutorial](https://markkarpov.com/tutorial/megaparsec.html#white-space).

### Parsing infixes

One problem with parser combinator libraries is that it's easy to get into an
infinite loop. `megaparsec` has a bunch of useful helpers for this, in
particular, support for `Operator`. 
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
```

A nice part of the the above code is that by putting `*` in a group before `+`
and `-`, we have defined operator precidence and so `6 * 1 + 1` equals `7` rather
than `12` (which we'd get from `6 * (1 + 1)`). 

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
internally. Why bother? Although it's pretty clear how this calculator should work, it
will be useful to compare our own simple interpreter with the LLVM output as
things get more complicated.

Fortunately, this isn't much of hardship:

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

As you can see, `EPrim` values don't really do anything.

When we see an `EInfix` value, we look for `EPrim` values inside and `add` / `subtract` / `multiply` them. If they're
not `EPrim` values yet, then they must be nested expressions, so we `interpret` them
and try again.

## Enough nonsense, let's do some compiling 

Hopefully we understand what our small language is now, so let's get down to
the business of turning it into real life native code.

### What is LLVM

It's a compiler backend, part of `clang`. It's very C shaped, and used by
`Rust` and `Swift` amongst others. It's got a bunch
of number types, structs, and arrays, functions, and enough pointers to have a
very bad time. Fortunately for now we won't need too much. 

### Our "runtime"

In order to _do things_ our language will need a runtime, This is a bunch of helper code written in something that be compiled to the target
language. Because we'll be compiling using `clang`, we'll use `C`. I don't know much about `C`, but what I can tell you is that this file contains a single function called `printint`, that takes an `int` and prints
it to `stdout`.

```c
#include <stdio.h>
#include <stdlib.h>

void printint(int i) {
  printf("%d", i);
}
```

This is the only `C` you'll need to see today. I'm sorry about that. Onwards!

### Our first LLVM module 

We'll be creating modules of `LLVM IR` (IR stands for Intermediate
Representation). There is a through explanation of module structure in the [LLVM language
reference](https://llvm.org/docs/LangRef.html#module-structure), however we
will only need the basics for now. 

To start us off, here is the representation for printing the number `42` to `stdout`.

```llvm
; ModuleID = 'calc'

declare external ccc i32 @printint(i32)

define external ccc i32 @main()    {
  %1 =  call ccc i32 @printint(i32  42)
  ret i32 0
}
```

Let's take this apart line by line:

---

```llvm
; ModuleID = 'calc'
```

This is a comment. See, LLVM is easy.

---

```llvm
declare external ccc i32 @printint(i32)
```

This declares an external function we'd like to use. In this case it's the
`printint` function from our standard library defined above. It
takes a single argument, an `i32`, and returns an `i32`.

---

```llvm
define external ccc i32 @main() {
```

This defines a function called `main` that takes no arguments and returns a
single `i32`.

The `ccc` part is the "calling convention". This defines the manner in which
LLVM will generate the function code. There are [absolutely loads of these](https://llvm.org/docs/LangRef.html#calling-conventions), many
designs to optimise specific languages. We'll keep to `ccc` for now, but may
want to change our minds when we start doing tail calls. 

---

```llvm
%1 =  call ccc i32 @printint(i32  42)
```

This line of defines a variable `%1`, and sets it the result of calling the
`printint` function we imported above. We pass the `printint` function a single
argument, an `i32` value of `42`. This will print the number `42` to stdout.

---

```llvm
ret i32 0
```

Functions in `LLVM` must return something (although that thing can be `null`).
This line returns an `i32` value of `0`. Because this is the `main` function of
our program, this is what will be returned to the operating system as our exit
code. `0` means "great job, everything went fine".

---

```llvm
}
```

This is a closing curly brace. `LLVM` is like `C`, we have to specifically
end our functions. No great hardship though.

---

> __💡 What is this `i32` business all about?💡__
>
> Because we're compiling towards native code, we need to be exact about our
> datatypes. You can see all LLVM's basic data types
> in the [language
> reference](https://llvm.org/docs/LangRef.html#single-value-types).
>
> `i32` is a 32-bit integer
> 
> `i1` is a 1-bit integer (or a `boolean`, as it can only be `0` or `1`)


### Compiling and running our hand-baked module 

To check that the code above does what we say it does, and to check we've got
everything installed that we need, let's write the LLVM to a file called
`module.ll`, the C code to `runtime.c`, and then compile it using `clang`.

> __💡 Who is Clang?💡__
>
> Clang is a C compiler than LLVM lives in. 

```bash
clang -Wno-override-module -lm module.ll runtime.c -o a.out 
```

This should output an executable called `a.out`. If we run it, we should see
this:

```bash
./a.out
42
```

Hooray!

## Compilng our actual programming language to LLVM 

OK. So now we've had a taste of the raw power available to us, let's make a
computer write this for us.

Fortunately, we do not need to hand output LLVM IR. We will use the following
libraries to do it for us:

[llvm-hs-pure](https://hackage.haskell.org/package/llvm-hs-pure) - a set of
types for LLVM IR, along with some helpful building functions.

[llvm-hs-pretty](https://hackage.haskell.org/package/llvm-hs-pretty) - a
prettyprinter for `llvm-hs-pure`.

We'll use this to write files and then compile by hand. This means that when we
invariably generate LLVM errors, we'll at least we able to look at the error
and reference the code we've given it to try and work out what went wrong.

### Shut up and show me some Haskell code

This is the entire module for creating an LLVM module from our `Expr` type:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Calc.Compile.ToLLVM (toLLVM) where

import Calc.Types
import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM

-- | given our `Expr` type, turn it into an LLVM module
toLLVM :: Expr ann -> LLVM.Module
toLLVM expr =
  LLVM.buildModule "example" $ do
    -- import `printint` from our standard library
    -- it takes an `i32` and returns an `i32`
    printInt <- LLVM.extern "printint" [LLVM.i32] LLVM.i32

    -- create a function called `main` that will be the entry point to our
    -- program
    LLVM.function "main" [] LLVM.i32 $ \_ -> do
      -- build the LLVM AST for our expression
      ourExpression <- exprToLLVM expr

      -- print our result to stdout
      _ <- LLVM.call printInt [(ourExpression, [])]

      -- return success exit code of `0`
      LLVM.ret (LLVM.int32 0)

exprToLLVM ::
  ( LLVM.MonadIRBuilder m,
    LLVM.MonadModuleBuilder m
  ) =>
  Expr ann ->
  m LLVM.Operand
exprToLLVM (EPrim _ prim) = 
  pure $ primToLLVM prim
exprToLLVM (EInfix _ OpAdd a b) = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.add lhs rhs
exprToLLVM (EInfix _ OpSubtract a b) = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.sub lhs rhs
exprToLLVM (EInfix _ OpMultiply a b) = do
  lhs <- exprToLLVM a
  rhs <- exprToLLVM b
  LLVM.mul lhs rhs

primToLLVM :: Int -> LLVM.Operand
primToLLVM i = LLVM.int32 (fromIntegral i)
```

Let's go through this.

### summing up 

write some stuff

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[mimsa](https://github.com/danieljharvey/mimsa)
