---
title: Compiling a functional language to LLVM, part 1 
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
concepts. The source code for the whole working calculator, along with a basic repl, can be found
at [llvm-calc](https://github.com/danieljharvey/mimsa/tree/trunk/llvm-calc).

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

We call datatypes like `Expr` _Abstract Syntax Trees_ (or ASTs).

You may notice the `ann` type variable. We'll use this to "decorate" items in
our AST. For now, we'll just use `()`.

## Parsing text into AST terms

Although we could provide our compiler as a Haskell library and ask our users
to manually create `Expr` values for us, it'd be much nicer to be able to read
input from a user.

This process is called _parsing_, and looks something like this:

```haskell
parseInput :: Text -> Either ParseError (Expr ())
```

Given some user input, parsing returns either a valid `Expr` or a (hopefully)
helpful error.

The whole parser can be seen
[here](https://github.com/danieljharvey/mimsa/tree/trunk/llvm-calc/src/Calc/Parser). We'll not go into too much depth as there are already lots of great references
on parsing, but a few details are worth noting:

### Which library?

We'll be using a library called
[megaparsec](https://hackage.haskell.org/package/megaparsec). It's fast, it's got a [great
tutorial](https://markkarpov.com/tutorial/megaparsec.html), and it generates errors that we can render nicely with the [diagnose](https://github.com/Mesabloo/diagnose) library. 

![A megaparsec parse error made nice with diagnose](/images/llvm-1-parse-error.png "A megaparsec parse error made nice with diagnose")

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
covered in much better detail in the
[megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html#white-space).

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

We won't be using these values today as our language is so limited that it's
pretty difficult to break, but as we add multiple types (and thus, the
possibility of type errors) we'll use them to show the user where they did a
boo boo.

## A simple interpreter

Before we get stuck into `LLVM`, it's good to be able to evaluate our language
internally. Although it's pretty clear how this calculator should work, it
will be useful to compare our own simple interpreter with the LLVM output as
things get more complicated.

You can view the full code
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc/src/Calc/Interpreter.hs).
As you can see, there isn't very much of it:

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
  -- one of our arguments isn't yet a number
  -- so simplify the leaves and try again
  interpretInfix ann op (interpret a) (interpret b)
```

When we see an `EInfix` value, we look for `EPrim` values inside and `add` / `subtract` / `multiply` them. If they're
not `EPrim` values yet, then they must be nested expressions, so we `interpret` them
and try again.

![A big calculator](/images/llvm-1-giant-calculator.jpg "A big calculator")

## Enough nonsense, let's do some compiling 

Hopefully we understand what our small language is now, so let's get down to
the business of turning it into real life native code.

### What is LLVM

LLVM stands for Low Level Virtual Machine. The idea is that higher level languages compile into LLVM, and then LLVM is turned into whatever local version of assembly is required. This means that by using LLVM, your programs will work on lots of architectures without you needing to understand a tremendous amount about them. As somebody who understands pretty much nothing about any processor architecture, this is very appealing indeed. If you need further persuading, `Rust`, `Swift` and `GHC` can all compile to
LLVM.

LLVM is part of the `clang` C compiler, and as a result, it's very C shaped.
It's got a bunch of number types, structs, and arrays, functions, and enough pointers to have a
very bad time.

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

LLVM is organised in [modules](https://llvm.org/docs/LangRef.html#module-structure).

To start us off, here is the IR (Intermediate Representation) for printing the number `42` to `stdout`.

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

### Compiling and running our hand-baked module 

To check that the code above does what we say it does, and to check we've got
everything installed that we need, let's write the LLVM to a file called
`module.ll`, the C code to `runtime.c`, and then compile it using `clang`.

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

## Compiling our programming language to LLVM 

OK. So now we've had a taste of the raw power available to us, let's get down
to business. Although we could just create raw LLVM IR by hand, instead we will use the following
libraries:

[llvm-hs-pure](https://hackage.haskell.org/package/llvm-hs-pure) - a set of
types for LLVM IR, along with some helpful building functions.

[llvm-hs-pretty](https://hackage.haskell.org/package/llvm-hs-pretty) - a
prettyprinter for `llvm-hs-pure`.

We'll use the datatypes in `llvm-hs-pure` to create modules, then use `llvm-hs-pretty` to render these to files and then compile by hand. This means that when we
invariably generate LLVM errors, we'll at least we able to look at the error
and reference the code we've given it to try and work out what went wrong.

### Shut up and show me some Haskell code

The whole module can be seen
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc/src/Calc/Compile/ToLLVM.hs), but let's look at the highlights:

---

```haskell
primToLLVM :: Int -> LLVM.Operand
primToLLVM i = LLVM.int32 (fromIntegral i)
```

This function creates integer literals from our `EPrim` constructors, using
the [int32](https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-IRBuilder-Constant.html#v:int32) function.
This returns an `Operand` (an LLVM value, broadly) that we can pass to other LLVM functions. We'll
expand this function to include more types as we need them.

---

```haskell
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
```

Here we compile our `Expr` into and take care of adding /
subtracting / multiplying integers.

Because we're using the `MonadIRBuilder`
and `MonadModuleBuilder`, it's almost as if we're writing code to do the
interpreting by hand, as all the actual code output is plumbed away in the
monad. If you squint, it looks very similar to the interpreter we wrote earlier, so if you
understand that, you pretty much understand this.

---

```haskell
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
```

Finally, we wrap up our compiled `Expr` and turn it into a
module. We are going to import `printint` from our standard library, output the
response of our computation to stdout, and then return exit code `0`. 

Let's look at the generated LLVM for `6 * 8 - 3`:

```llvm
; ModuleID = 'example'

declare external ccc i32 @printint(i32)

define external ccc i32 @main() {
  %1 = mul   i32 6, 8
  %2 = sub   i32 %1, 3
  %3 =  call ccc  i32  @printint(i32  %2)
  ret i32 0
}
```

Hopefully, nothing too surprising.

## Running our code

As with earlier, our little compiler works by pretty printing the LLVM module
we created, then running `clang` to compile it into an executable. I pretty
much stole the compiling code from
[Micro-C](https://blog.josephmorag.com/posts/mcc3/) (thanks/sorry, Joseph!). 
You can see our slightly tattered version
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc/src/Calc/Compile/RunLLVM.hs) if for some reason you don't believe any of this actually works.

### Well that's that 

Congratulations, you are all low-level compiler experts now. Hopefully that was
somewhat helpful. Next time we'll be adding the equality operator and some basic control flow.
Exciting!

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[llvm reference](https://llvm.org/docs/LangRef.html)

[mimsa](https://github.com/danieljharvey/mimsa)

