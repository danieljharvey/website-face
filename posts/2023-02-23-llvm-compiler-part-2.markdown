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

Let's do that now! 

```haskell
-- | operators for combining expressions
data Op
  = OpAdd
  | OpMultiply
  | OpSubtract
  | OpEquals -- this is new! 
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
we'll never get anywhere). This means we'll need to extend our `Prim` type to also describe `Boolean` values as
well as `Integer`s.

```haskell
-- | types of basic values
data Prim
  = PInt Integer
  | PBool Bool
  deriving stock (Eq, Ord, Show)
```

However this means we are in danger of our users being able to make silly mistakes like `if 27 then False else 6`? How can we stop this? This can only mean one thing: we are going to need
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
`Expr ann` and returns either `Expr (Type ann)` or an excuse. It starts by
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
inferIf :: ann -> Expr ann -> Expr ann -> Expr ann -> Either (TypeError ann) (Expr (Type ann))
inferIf ann predExpr thenExpr elseExpr = do
  predA <- infer predExpr
  case getOuterAnnotation predA of
    (TPrim _ TBool) -> pure ()
    otherType -> throwError (PredicateIsNotBoolean ann otherType)
  thenA <- infer thenExpr
  elseA <- check (getOuterAnnotation thenA) elseExpr
  pure (EIf (getOuterAnnotation elseA) predA thenA elseA)
```

This is how `if` works. We `infer` the type of the predicate, then use
`getOuterAnnotation` to get the `Type ann` out of it. We then case match on it
to see if it's a `Boolean` or not, "throwing" an error if not.

---

```haskell
inferInfix ::
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  Either (TypeError ann) (Expr (Type ann))
inferInfix ann OpEquals a b = do
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
```

When typechecking `==`, we want to make sure both sides have the same type,
"throwing" an error if not.

---

```haskell
inferInfix ann op a b = do
  elabA <- infer a
  elabB <- infer b
  -- all the other infix operators need to be Int -> Int -> Int
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

Here are the other operators. Both the arguments should be `Integer` and the
return type is `Integer`, otherwise we construct and return an error type. It
seems like a lot of work to be so specific, but look how helpful our errors
are!

![Nice!](/images/llvm-2-type-error-1.png "Nice!")

![Great!](/images/llvm-2-type-error-2.png "Great!")

![Tremendous!](/images/llvm-2-type-error-3.png "Tremendous!")

![Whoa!](/images/llvm-2-type-error-4.png "Whoa!")

![What?](/images/llvm-2-type-error-5.png "What?")

![Absolutely!](/images/llvm-2-type-error-6.png "Absolutely!")

![Surely not?](/images/llvm-2-type-error-7.png "Surely not?")

![Ridiculous!](/images/llvm-2-type-error-8.png "Ridiculous!")

![Huh?](/images/llvm-2-type-error-9.png "Huh?")

You can see all the error rendering code
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc2/src/Calc/Typecheck/Error.hs).

---

```haskell
infer :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
infer (EPrim ann prim) =
  pure (EPrim (typeFromPrim ann prim) prim)
infer (EIf ann predExpr thenExpr elseExpr) =
  inferIf ann predExpr thenExpr elseExpr
infer (EInfix ann op a b) =
  inferInfix ann op a b
```

That's how we put `infer` together, easy!

---

```haskell
check :: Type ann -> Expr ann -> Either (TypeError ann) (Expr (Type ann))
check ty expr = do
  exprA <- infer expr
  if void (getOuterAnnotation exprA) == void ty
    then pure (expr $> ty)
    else throwError (TypeMismatch ty (getOuterAnnotation exprA))
```

Lastly, here's `check`. We only use it when comparing arms of `if` statements,
but soon this will become more interesting.

You can see all of the typechecker code
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc2/src/Calc/Typecheck/Elaborate.hs).

---

## Interpreting our new friends

Before heading back into LLVM land, let's update our manual interpreter so we
can understand what's needed here.

Firstly, it's now possible that our interpreter can fail. This will only happen
if our typechecker is not working as expected, but we should make a proper
error type for it anyway because we are good programmers who care about our
users.

```haskell
data InterpreterError ann
  = NonBooleanPredicate ann (Expr ann)
  deriving stock (Eq, Ord, Show)
```

Interpreting infix expressions is a little bit more complicated, as our pattern
matches have to make sure we're looking at the right `Prim` values. The
eagle-eyed may notice that a broken typechecker could send this into a loop.
Can you see where?

```haskell
interpretInfix ::
  (MonadError (InterpreterError ann) m) =>
  ann ->
  Op ->
  Expr ann ->
  Expr ann ->
  m (Expr ann)
interpretInfix ann OpAdd (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a + b)
interpretInfix ann OpSubtract (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a - b)
interpretInfix ann OpMultiply (EPrim _ (PInt a)) (EPrim _ (PInt b)) =
  pure $ EPrim ann (PInt $ a * b)
interpretInfix ann OpEquals (EPrim _ a) (EPrim _ b) =
  pure $ EPrim ann (PBool $ a == b)
interpretInfix ann op a b = do
  iA <- interpret a
  iB <- interpret b
  interpretInfix ann op iA iB
```

We ended up with a `MonadError` constraint above - why's that? It's because the
main `interpret` function can now "explode" if we try and match a non-predicate
in an if statement. Our typechecker _should_ stop this happening of course.

```haskell
-- | just keep reducing the thing until the smallest thing
interpret ::
  ( MonadError (InterpreterError ann) m
  ) =>
  Expr ann ->
  m (Expr ann)
interpret (EPrim ann p) = pure (EPrim ann p)
interpret (EInfix ann op a b) =
  interpretInfix ann op a b
interpret (EIf ann predExpr thenExpr elseExpr) = do
  predA <- interpret predExpr
  case predA of
    (EPrim _ (PBool True)) -> interpret thenExpr
    (EPrim _ (PBool False)) -> interpret elseExpr
    other -> throwError (NonBooleanPredicate ann other)
```

We interpret if statements by reducing the predicate down to a boolean, then
taking a peek, and then interpreting the appropriate branch. If we don't need a
branch, there's no need to interpret it!

You can see all of the interpreter code
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc2/src/Calc/Interpreter.hs).

## OK, LLVM time

I feel like I'm rushing through all this, and maybe copy pasta-ing an entire
typechecker in the preamble was somewhat undisclined of me.

BUT, here we go.

### Digression

Firstly, we'll add a new function to our C "standard library":

```c
void printbool(int b) {
  printf(b ? "True" : "False");
}
```

It will take an LLVM boolean, and print either `True` or `False` depending on
whether it is `0` or not.

### To the IR!

We're going to start by looking at the LLVM IR for the following arbitrary
expression:

```bash
if 2 == 1 then True else False 
```

```llvm
; ModuleID = 'example'

declare external ccc  void @printbool(i1)

define external ccc  i32 @main()    {
  %1 = icmp eq i32 2, 1
  %2 = alloca i1
  br i1 %1, label %then_0, label %else_0
then_0:
  store   i1 1, i1* %2
  br label %done_0
else_0:
  store   i1 0, i1* %2
  br label %done_0
done_0:
  %3 = load   i1, i1* %2
   call ccc  void  @printbool(i1  %3)
  ret i32 0
}
```

What a ride! Let's take it line by line.

---

```llvm
; ModuleID = 'example'
```

Once again, let's ease ourselves in with a code comment.

---

```llvm
declare external ccc  void @printbool(i1)
```

Declaration for the new function in our standard library. It takes an `i1` (a
boolean, stored as `0` or `1`) and returns `void`.

---

```llvm
define external ccc  i32 @main()    {
```

We define the `main` function, which is the entry point of our program. It
takes no arguments, and returns an `i32` integer value (which becomes the exit
code).

---

```llvm
%1 = icmp eq i32 2, 1
```

Here we are making a new variable, `%1`, by comparing two integers, `2` and
`1`, using `eq`. This is our `2 == 1` expression, and maps across quite neatly.

---

```llvm
%2 = alloca i1
```

To make control flow works, we are going to need to jump to different places.
However, LLVM has no way of passing a value back between sections. Therefore,
we are going to create a mutable placeholder for the result, and each branch
will be responsible for storing the result here. `alloca` is broadly "allocate
memory" and `i1` is the LLVM type for a `Boolean`.

---

```llvm
br i1 %1, label %then_0, label %else_0
```

This is where we do the branching. `br` takes an `i1` value for the predicate,
and then two labels for blocks that we'll jump to depending on the value of the predicate.
Therefore if `%1` is `1` we'll jump to `then_0`, otherwise we'll jump to
`else_0`. We'll define these shortly.

---

```llvm
then_0:
  store   i1 1, i1* %2
  br label %done_0
```

This defines a block labelled `then_0`. We will "jump" here in the "then" case
of the if statement. We store `1` in the `%2` variable, and then jump to the
`done_0` block.

---

```llvm
else_0:
  store   i1 0, i1* %2
  br label %done_0
```

This defines a block labelled `else_0`. We will "jump" here in the "else" case.
Once again, we store `0` in the `%2` variable, and then jump to `done_0`.

---

```llvm
done_0:
  %3 = load   i1, i1* %2
```

This introduces a new block called `done_0`. As our if construct is an
expression, we always need to return something, so ee jump here when the
`then` or `expr` branches are finishing doing their business, and load whatever
they stored in `%2`.

---

```llvm
call ccc  void  @printbool(i1  %3)
```

Call the `printbool` function from our standard library with the loaded value.

---

```llvm
ret i32 0
```

As our program succeeded, we return a `0`, this becomes our exit code.

---

```llvm
}
```

As a little palette cleanser, a nice closing brace.

### Generating it from Haskell

Now we have `Boolean` as well as `Integer` values, we'll need to represent them
in LLVM. We'll use a `bit` which is a 1-bit LLVM number to represent
`Boolean`s.

```haskell
primToLLVM :: Prim -> LLVM.Operand
primToLLVM (PInt i) = LLVM.int32 (fromIntegral i)
primToLLVM (PBool True) = LLVM.bit 1
primToLLVM (PBool False) = LLVM.bit 0
```

Now we'll need to choose the right printing function:

```haskell
-- import the correct output function from our standard library
-- depending on the output type of our expression
printFunction :: (LLVM.MonadModuleBuilder m) => Type ann -> m LLVM.Operand
printFunction (TPrim _ TInt) = LLVM.extern "printint" [LLVM.i32] LLVM.void
printFunction (TPrim _ TBool) = LLVM.extern "printbool" [LLVM.i1] LLVM.void
```

The most interesting part is `if` expressions. We use the `RecursiveDo`
extension, which gives us the `mdo` syntax. This lets us use bindings before
they are created. This will allow us to use `thenBlock` and `elseBlock` before
they're defined. We create IR for the `predExpr`, then pass it to
`LLVM.condBr`, which will then jump to the appropriate block depending on the
value.

```haskell
ifToLLVM ::
  (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) =>
  Type ann ->
  Expr (Type ann) ->
  Expr (Type ann) ->
  Expr (Type ann) ->
  m LLVM.Operand
ifToLLVM tyReturn predExpr thenExpr elseExpr = mdo
  -- create IR for predicate
  irPred <- exprToLLVM predExpr

  -- make variable for return value
  irReturnValue <- LLVM.alloca (typeToLLVM tyReturn) Nothing 0

  -- this does the switching
  -- we haven't created these blocks yet but RecursiveDo lets us do this with
  -- MonadFix magic
  LLVM.condBr irPred thenBlock elseBlock

  -- create a block for the 'then` branch
  thenBlock <- LLVM.block `LLVM.named` "then"
  -- create ir for the then branch
  irThen <- exprToLLVM thenExpr
  -- store the result in irResultValue
  LLVM.store irReturnValue 0 irThen
  -- branch back to the 'done' block
  LLVM.br doneBlock

  -- create a block for the 'else' branch
  elseBlock <- LLVM.block `LLVM.named` "else"
  -- create ir for the else branch
  irElse <- exprToLLVM elseExpr
  -- store the result in irReturnValue
  LLVM.store irReturnValue 0 irElse
  -- branch back to the `done` block
  LLVM.br doneBlock

  -- create a block for 'done' that we always branch to
  doneBlock <- LLVM.block `LLVM.named` "done"
  -- load the result and return it
  LLVM.load irReturnValue 0
```

To work out which kind of type to `alloca`, we take the return type and use it
to work which LLVM type to use.

```haskell
typeToLLVM :: Type ann -> LLVM.Type
typeToLLVM (TPrim _ TBool) = LLVM.i1
typeToLLVM (TPrim _ TInt) = LLVM.i32
```

You can see all of the LLVM conversion code
[here](https://github.com/danieljharvey/mimsa/blob/trunk/llvm-calc2/src/Calc/Compile/ToLLVM.hs).

---

### Well that's that 

Congratulations, you are all bidirectional type checking experts now. Hopefully that was
somewhat helpful. Next time we'll be adding basic functions and variables.
Great!

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[llvm reference](https://llvm.org/docs/LangRef.html)

[mimsa](https://github.com/danieljharvey/mimsa)

