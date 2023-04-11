---
title: Compiling a functional language to LLVM, part 3 
tags: plt, mimsa, typechecker, llvm 
---

Welcome to part 3 of this series in compiling functional languages to LLVM. In
[part 1](/posts/2023-02-23-llvm-compiler-part-2.html) we added basic control
flow to our langauge with an `if / then / else` construct.

Today we're going to get one important step towards a functional programming
language by adding functions and variables. Unfortunately these won't be lambdas (yet), but
they are one important step towards them: 

By the end of today we'll be able to make small modules such as:

```bash
function add(a: integer, b: integer) {
  a + b
}

function increment(a: integer) {
  a + 1
}

add(increment(1), add(2, 3))
```

## No lambdas, what gives?

LLVM is part of the `Clang` C compiler, so the IR is pretty C shaped, all told. All [functions](https://llvm.org/docs/LangRef.html#functions) in LLVM must be defined at the top level with static types. This means that we could model an expression like:

```haskell
let my_func = \(a: Integer) -> a + 1;

my_func(41)
```

quite easily, as it turns into something like:

```typescript
function my_func(a: Integer) {
  return a + 1
}

function main() {
  my_func(41) 
}
```

However, if our lambda function uses a variable defined outside the function,
like `a` is here:

```haskell
let a = 100;

let my_func = \\b -> a + b;

my_func(41)
```

...we'd struggle a bit more: 

```typescript
function my_func(b: Integer) {
  // where do we get `a` from?
  return a + b
}

function main() {
  let a = 100;
  my_func(41) 
}
```

The problem is that the `my_func` lambda has "captured" the `a` value from it's
scope. Therefore, when calling it, we need to pass it this captured value as
well as the lambda argument `b`.

In pseudo code, it looks like this:

```typescript
function my_func(b: Integer, environment: {a: Integer}) {
  // `a` comes from the 'captured environment'
  return environment.a + b
}

function main() {
  let a = 100;

  // capture the environment that will be used by `my_func`
  let env = {a:100}

  // get a pointer to `my_func` so we can pass it around as a first class value
  let pointer_to_my_func = my_func

  // call the function using the pointer, passing any args plus the environment
  pointer_to_my_func(41, env)
}
```

Today we're going to implement basic functions, and we'll move onto closures in
the next chapter.

## OK, lets get concrete

We're going to need some new datatypes to express all the new things we want to
do.

### Function

Firstly, we have a `Function` type.

```haskell
data Function ann = Function
  { fnAnn :: ann,
    fnArgs :: [(ArgumentName, Type ann)],
    fnFunctionName :: FunctionName,
    fnBody :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)
```

This lets us describe something like: 

```bash
function add(a: Integer, b: Integer) { 
  a + b 
}
```

Note that the function body is a single `Expr`, and that it can use variables
`a` and `b` introduced as function arguments.

The `ann` type will contain file location information
after parsing, and then will contain the type of the function after
typechecking.

As mentioned above, our function implementation is very limited - we can only
use variables passed into the function as arguments.

### Module

A `Module` lets us combine multiple functions and a `main` expression.

```haskell
data Module ann = Module
  { mdFunctions :: [Function ann],
    mdExpr :: Expr ann
  }
  deriving stock (Eq, Ord, Show, Functor)
```

This lets us write code like:

```bash
function increment(a: Integer) {
  a + 1
}

function decrement(a: Integer) {
  a - 1
}

increment(decrement(1)) == 1
```

Our implementation is quite limited. Functions can only be used in the order
they are defined. Therefore `decrement` could call `increment`, but not the
other way round. We can improve this in future with some basic dependency
analysis. 

### Expr

Our `Expr` needs a couple of new constructors. 

```haskell
data Expr ann
  = EPrim ann Prim
  | EInfix ann Op (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann)
  | EVar ann Identifier -- new!
  | EApply ann FunctionName [Expr ann] -- new!
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
```

`EVar` represents a variable such as `a`, and `EApply` represents function application (`add(1,2)`).

## Typechecker changes

After a rather sizable swerve into bidirectional typechecking in the previous
part, we are going to focus less on the changes here.

The most important part is that it's become stateful, as we will be learning
about both functions and variables as we typecheck. We have created a
`TypecheckM` newtype that we use, that contains both a `ReaderT` and a
`StateT`.

```haskell
newtype TypecheckEnv ann = TypecheckEnv
  { tceVars :: HashMap Identifier (Type ann)
  }
  deriving stock (Eq, Ord, Show)

newtype TypecheckState ann = TypecheckState
  {tcsFunctions :: HashMap FunctionName (Type ann)}
  deriving stock (Eq, Ord, Show)

newtype TypecheckM ann a = TypecheckM
  { getTypecheckM ::
      ReaderT (TypecheckEnv ann)
          (StateT (TypecheckState ann) 
            (Either (TypeError ann))) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (TypecheckEnv ann),
      MonadError (TypeError ann),
      MonadState (TypecheckState ann)
    )
```

The reasons for separate `Reader` and `State` are the nature of the state in
them. Variables only live for the life of a function that are defined in, so
they live in the `TypecheckEnv` used by `Reader`, and disappear after the
function definition is typechecked ([more information on this technique
here](https://blog.cofree.coffee/2021-08-13-that-one-cool-reader-trick/)).

We use `TypecheckState` and `State` for the functions we create as we want them
to accumulate and stay in scope for the rest of typechecking as we use them.

These types, and the functions used to store / fetch variables and functions
are defined
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc3/src/Calc/Typecheck/Types.hs).

### Elaborating a function

Our functions have types for the arguments, so we push them into the `Reader`
environment, and then elaborate the expression inside.

```haskell
elaborateFunction ::
  Function ann ->
  TypecheckM ann (Function (Type ann))
elaborateFunction (Function ann args name expr) = do
  exprA <- withFunctionArgs args (infer expr)
  let argsA = fmap (second (\ty -> fmap (const ty) ty)) args
  let tyFn = TFunction ann (snd <$> args) (getOuterAnnotation exprA)
  pure (Function tyFn argsA name exprA)
```

We've extended the `Type` datatype to add a `TFunction` constructor, which
contains the types of all the arguments, and the return type. All functions
will have a `TFunction` type.

```haskell
data Type ann
  = TPrim ann TypePrim
  | TFunction ann [Type ann] (Type ann) -- new!
  deriving stock (Eq, Ord, Show, Functor)
```

### Elaborating a module

Elaborating a module involves:

- Elaborate each function
- Push it's type into the `State`
- Elaborate the `main` expression

```haskell
elaborateModule ::
  forall ann.
  Module ann ->
  Either (TypeError ann) (Module (Type ann))
elaborateModule (Module {mdFunctions, mdExpr}) = runTypecheckM (TypecheckEnv mempty) $ do
  fns <-
    traverse
      ( \fn -> do
          elabFn <- elaborateFunction fn
          storeFunction (fnFunctionName elabFn) (fnAnn elabFn)
          pure elabFn
      )
      mdFunctions

  Module fns <$> infer mdExpr
```

## Updating the interpreter

We won't go into the interpreter changes today, they work in the same way as
the typechecker,
storing variables in the `Reader` env and functions in the `State`. [The code
lives
here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc3/src/Calc/Interpreter.hs). 

## To the IR!

Here is an expression:

```bash
function sum(a: Integer, b: Integer) {
  a + b
}

sum(20, 22)
```

Here is the LLVN output for it. Hopefully it's not too brutal.

```llvm
; ModuleID = 'example'

declare external ccc  void @printint(i32)

define external ccc  i32 @sum(i32  %a_0, i32  %b_0)    {
  %1 = add   i32 %a_0, %b_0
  ret i32 %1
}

define external ccc  i32 @main()    {
  %1 =  call ccc  i32  @sum(i32  20, i32  22)
  call ccc  void  @printint(i32  %1)
  ret i32 0
}
```


```llvm
; ModuleID = 'example'
```

A comment, lol.

```llvm
declare external ccc  void @printint(i32)
```

We define our output function from our standard library.

```llvm
define external ccc  i32 @sum(i32  %a_0, i32  %b_0)    {
```

Define the function. The first `i32` is the return type. `%a_0` is the first
argument with type `i32`, and `%b_0` is the second argument, with type `i32`.

```llvm
  %1 = add   i32 %a_0, %b_0
  ret i32 %1
}
```

This is the function body. We add `%a_0` and `%b_0`, and then return it.

define external ccc  i32 @main()    {
  %1 =  call ccc  i32  @sum(i32  20, i32  22)
  call ccc  void  @printint(i32  %1)
  ret i32 0
}
```


