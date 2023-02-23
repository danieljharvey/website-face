---
title: Compiling a functional language to LLVM, part 2 
tags: plt, mimsa, typechecker, llvm 
---

Welcome to part 2 of this series in compiling functional languages to LLVM. In
[part 1](/posts/2023-02-08-llvm-compiler-part-1.html) we created a very simple
calculator that let us add, subtract and multiply integers like `1 + 1` or `6 *
(5 - 2)`.

Today we're going to spice things up a touch by adding some basic control flow.
By the end of today we're going to be writing sweet syntax such as:

```bash
2 + 2 == 5

if 6 == 6 then False else True
```

## What's new then?

To make our dreams come true, we're going to two new
syntactic features:

- `if` expressions 

- an `==` infix operator

Let's add those to our `Expr` type now:

```haskell
-- | operators for combining expressions
data Op
  = OpAdd
  | OpMultiply
  | OpSubtract
  | OpEquals -- this is new! 
  deriving stock (Eq, Ord, Show)

-- | types of basic values
data Prim
  = PInt Integer
  | PBool Bool
  deriving stock (Eq, Ord, Show)

-- | Expressions, decorated with some unknown `ann`
data Expr ann
  = EPrim ann Prim -- this `Prim` used to always be `Int`
  | EInfix ann Op (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann) -- this is new!
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
```

Our `EIf` constructor takes three `Expr ann` as arguments. The first is the
`predicate`, ie, the thing that must evaluate to `True` or `False`, and the
other two are expressions to be evaluated on the `then` and `else` branches.
For all of this to make any sense, these will need to be the same type.

## Types?

Previously we sort of glossed over the idea of types, because every value in
our calculator was either an `Integer` (ie, `1`, `42`) or an expression that would eventually
evaluate into an `Integer` (like `1 + 1`, `6 * 12`).

However, the expression `1 == 1` doesn't resolve to an `Integer`, it can only
be `True` or `False`, ie a `Boolean` type. (it is true that we could express
this with an `Integer` but if we start cutting corners this early in the game
we'll never get anywhere). This can only mean one thing: we are going to need
to write a bidirectional type checker.

### Bidirect what?

A bidirectional type checker is a way of working which types parts of an
expression has, and identifying parts that don't make sense. What makes it
"bidirectional" is that it works in two "modes":

- `infer` mode: given an expression, give me the type
- `check` mode: given an expression and the type we think it has, give me the
  type

The broad idea is when we don't know anything about an expression, we `infer`
what types it has, but as we learn more, we use that information to help us
work the rest out. Although we could arguably get away with just an `infer` mode for a language
this simple, we will need this special two-way magic in future.

Enough waffle, let's see some code, and then talk about it.

### Code

Firstly, we need a type for types. We'll call it `Type`. We are adding an `ann`
type argument to it, so that we can attach source code locations etc. This will
be helpful for showing our user helpful errors, which we will be doing today:

```haskell
data TypePrim = TBool | TInt
  deriving stock (Eq, Ord, Show)

-- the `ann` is used to attach source code location etc
data Type ann
  = TPrim ann TypePrim
  deriving stock (Eq, Ord, Show, Functor)
```

### Things go wrong

Any old typechecker can tell you when things are going well, but the ones that
are really worth their salt are the ones that tell you helpfully what is going
wrong. We will need these ones:

```haskell
data TypeError ann
  = PredicateIsNotBoolean ann (Type ann)
  | InfixTypeMismatch Op [(Type ann, Type ann)]
  | TypeMismatch (Type ann) (Type ann)
  deriving stock (Eq, Ord, Show)
```

### The meat of the thing

Therefore, the point of the typechecker is to take `Expr ann` (ie, an `Expr`
carrying around some `ann` that does not concern us), and turn it into either
`TypeError ann` or `Expr (Type ann)`.

`Expr (Type ann)` means that we'll have "decorated" each part of the `Expr`
with it's type. We'll take whatever `ann` was in there and put it in the `Type`
instead. For example: 

```haskell
oneWhichIsAnInteger :: Expr (Type ())
oneWhichIsAnInteger = EPrim (TPrim () TInt) (PInt 1)
```

The entire typechecker lives
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc2/src/Calc/Typecheck/Elaborate.hs).
Let's go through the key parts:

---

```haskell
-- | this is the function we run
elaborate :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
elaborate = infer -- start with `infer` because we know nothing
```

`elaborate` is the function the typechecker exports. It takes an untypechecked
``Expr ann` and returns either `Expr (Type ann)` or an excuse. It starts by
running `infer`, which we'll see shortly. 

---

```haskell
typeFromPrim :: ann -> Prim -> Type ann
typeFromPrim ann (PInt _) = TPrim ann TInt -- It's an Integer!
typeFromPrim ann (PBool _) = TPrim ann TBool -- It's a Boolean!
```

The most basic type inference we can do is looking at a primitive value. As it
stands in our language, there is one number type and one boolean type, so we
can unambiguously work out the type just by looking at the value.

---

```haskell
infer :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
infer (EPrim ann prim) =
  pure (EPrim (typeFromPrim ann prim) prim)
infer (EIf ann predExpr thenExpr elseExpr) = do
  predA <- infer predExpr
  case getOuterAnnotation predA of
    (TPrim _ TBool) -> pure ()
    otherType -> throwError (PredicateIsNotBoolean ann otherType)
  thenA <- infer thenExpr
  elseA <- check (getOuterAnnotation thenA) elseExpr
  pure (EIf (getOuterAnnotation elseA) predA thenA elseA)
infer (EInfix ann OpEquals a b) = do
  elabA <- infer a
  elabB <- infer b
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ tA, TPrim _ tB)
      | tA == tB ->
          -- if the types are the same, then great! it's a bool!
          pure (TPrim ann TBool)
    (otherA, otherB) ->
      -- otherwise, error!
      throwError (TypeMismatch otherA otherB)
  pure (EInfix ty OpEquals elabA elabB)
infer (EInfix ann op a b) = do
  elabA <- infer a
  elabB <- infer b
  ty <- case (getOuterAnnotation elabA, getOuterAnnotation elabB) of
    (TPrim _ TInt, TPrim _ TInt) ->
      -- if the types are the same, then great! it's an int!
      pure (TPrim ann TInt)
    (TPrim _ TInt, other) ->
      throwError
        ( InfixTypeMismatch
            op
            [ ( TPrim (getOuterTypeAnnotation other) TInt,
                other
              )
            ]
        )
    (other, TPrim _ TInt) ->
      throwError
        ( InfixTypeMismatch
            op
            [ ( TPrim (getOuterTypeAnnotation other) TInt,
                other
              )
            ]
        )
    (otherA, otherB) ->
      -- otherwise, error!
      throwError
        ( InfixTypeMismatch
            op
            [ (TPrim (getOuterTypeAnnotation otherA) TInt, otherA),
              (TPrim (getOuterTypeAnnotation otherB) TInt, otherB)
            ]
        )
  pure (EInfix ty op elabA elabB)
```

Whoa.

---

```haskell
check :: Type ann -> Expr ann -> Either (TypeError ann) (Expr (Type ann))
check ty expr = do
  exprA <- infer expr
  if void (getOuterAnnotation exprA) == void ty
    then pure (expr $> ty)
    else throwError (TypeMismatch ty (getOuterAnnotation exprA))

```

### Well that's that 

Congratulations, you are all bidirectional type checking experts now. Hopefully that was
somewhat helpful. Next time we'll be adding basic functions and variables.
Great!

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[llvm reference](https://llvm.org/docs/LangRef.html)

[mimsa](https://github.com/danieljharvey/mimsa)

