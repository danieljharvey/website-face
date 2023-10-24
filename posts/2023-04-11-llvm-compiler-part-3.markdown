---
title: Compiling a functional language to LLVM, part 3 
tags: plt, mimsa, typechecker, llvm 
---

Welcome to part 3 of this series in compiling functional languages to LLVM. In
[part 2](/posts/2023-02-23-llvm-compiler-part-2.html) we added basic control
flow to our langauge with an `if / then / else` construct.

Today we're going to get one important step towards a functional programming
language by adding functions and variables. 

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

However, there are quite a few things our functions _won't_ do.
 
+ They can't call themselves recursively (ie, `function factorial(a: Integer) { if a == 0 then 1 else a * factorial(a - 1) }`. This is to simplify the typechecker
  implementation for the time being rather than any limitation in LLVM - we'll come back round to making this
  possible.
+ We must define all function arguments up front (ie, `a: Integer, b:
  Boolean`). It is possible to infer these, but let's keep things simple for
  now.
+ Functions can only call functions defined before them. This can be solved by
  doing some dependency analysis before typechecking (ie, move things around to
  typecheck them in a sensible order). We avoid this for now, but will come
  back to it.
+ Functions can't return other functions, they can only be defined at the top
  level. This is a limitation of LLVM, however in future chapters we'll work around this by
  implementing closures.

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

Our function implementation is very limited - we can only
use variables passed into the function as arguments, and must explicitly
annotate each function argument with it's type.

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

As mentioned earlier, functions can only be used in the order
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

![Horse break](/images/horse3.jpg "Horse break")

## Typechecker changes

After a rather sizable swerve into bidirectional typechecking in the previous
part, we are going to focus less on the changes here.

### Adding state

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

The trick is using the `local` function from `Control.Monad.Reader`:

```haskell
withFunctionArgs :: [(Identifier, Type ann)] -> TypecheckM ann a -> TypecheckM ann a
withFunctionArgs args computation =
  local
    ( \tce ->
        tce
          { tceVars = tceVars tce <> HM.fromList args
          }
    )
    computation
```

We pass in some `args`, which are the function arguments and their types, and
`computation`, which is whatever typechecking we'd like do. Then throughout
running `computation`, we'll have extra variables in scope, and then they'll
disappear again. This is helpful for typechecking functions, where the vars
only exist inside.

### Inference changes

We have two new `infer` cases, `EVar` and `EApply`. `EVar` is pretty
straightforward:

```haskell
infer (EVar ann var) = do
  ty <- lookupVar ann var
  pure (EVar ty var)
```

We lookup the type for `var`, and decorate the type with it. If `lookupVar`
fails, it "throws" a `TypeError ann`.

```haskell
-- | look up a saved identifier "in the environment"
lookupVar :: ann -> Identifier -> TypecheckM ann (Type ann)
lookupVar ann identifier = do
  maybeType <- asks (HM.lookup identifier . tceVars)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allIdentifiers <- asks (HM.keysSet . tceVars)
      throwError (VarNotFound ann identifier allIdentifiers)
```

The other new `infer` case is `EApply`. This is used to get the type of an
applied function.

```haskell
infer (EApply ann fnName args) = do
  -- lookup function by name in State
  fn <- lookupFunction ann fnName
  (ty, elabArgs) <- case fn of
    TFunction _ tArgs tReturn -> do
      -- check the arguments length match the function 
      when
        (length args /= length tArgs)
        (throwError $ 
            FunctionArgumentLengthMismatch ann 
              (length tArgs) 
              (length args)
        )
      -- check each arg against type
      elabArgs <- zipWithM check tArgs args      
      -- return type and elaborated arguments
      pure (tReturn, elabArgs)
    _ -> throwError $ NonFunctionTypeFound ann fn
  pure (EApply (ty $> ann) fnName elabArgs)
```

Note how we use `check` here to check each argument against the expected type in the
function. This is where the bidirectional type checking approach really starts
to shine, as any problems become immediately apparent.

The `lookupFunction` helper is very similar to `lookupVar`, except it looks in
the `State` instead of `Reader`:

```haskell
-- | look up a saved identifier "in the environment"
lookupFunction :: ann -> FunctionName -> TypecheckM ann (Type ann)
lookupFunction ann fnName = do
  maybeType <- gets (HM.lookup fnName . tcsFunctions)
  case maybeType of
    Just found -> pure found
    Nothing -> do
      allFunctions <- gets (HM.keysSet . tcsFunctions)
      throwError (FunctionNotFound ann fnName allFunctions)
```

These types, and the functions used to store / fetch variables and functions
are defined
[here](https://github.com/danieljharvey/llvm-calc/blob/trunk/llvm-calc3/src/Calc/Typecheck/Types.hs).

### Elaborating a function

Our functions have types for the arguments, so we push them into the `Reader`
environment, and then elaborate the expression inside.

```haskell
elaborateFunction ::
  Function ann ->
  TypecheckM ann (Function (Type ann))
elaborateFunction (Function ann args name expr) = do
  -- with the `args` added to the Reader, infer the type of `expr`
  exprA <- withFunctionArgs args (infer expr)
  -- adjust the types of the arguments
  let argsA :: [(ArgumentName, Type (Type ann))]
      argsA = fmap (second (\ty -> fmap (const ty) ty)) args
  -- create type of function
  let tyFn = TFunction ann (snd <$> args) (getOuterAnnotation exprA)
  -- wrap it all back up again
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
  -- typecheck all functions...
  fns <-
    traverse
      ( \fn -> do
          -- typecheck function
          elabFn <- elaborateFunction fn
          -- add it to State
          storeFunction (fnFunctionName elabFn) (fnAnn elabFn)
          -- return it
          pure elabFn
      )
      mdFunctions
  
  -- typecheck `expr`, and wrap everything back together
  Module fns <$> infer mdExpr
```

![Horse break](/images/horse1.jpg "Horse break")

## Updating the interpreter

We won't go into the interpreter changes today, they work in the same way as
the typechecker,
storing variables in the `Reader` env and functions in the `State`. [The code
lives
here](https://github.com/danieljharvey/llvm-calc/blob/trunk/llvm-calc3/src/Calc/Interpreter.hs). 

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

---

```llvm
declare external ccc  void @printint(i32)
```

We define our output function from our standard library.

---

```llvm
define external ccc  i32 @sum(i32  %a_0, i32  %b_0)    {
```

Define the function. The first `i32` is the return type. `%a_0` is the first
argument with type `i32`, and `%b_0` is the second argument, with type `i32`.

---

```llvm
  %1 = add   i32 %a_0, %b_0
```

Body of the `sum` function, add `%a_0` and `%b_0` and assign the result to
`%1`.

---

```llvm
  ret i32 %1
```

Return `%1` from the function.

```llvm
}
```

End of `sum` function.

---

```llvm
define external ccc  i32 @main()    {
```

This defines the `main` function, the entry point to our application.

---

```llvm
%1 =  call ccc  i32  @sum(i32  20, i32  22)
```

Body of the `main` function - this calls the `sum` function, passing it `20`
amd `22` as args. The result is stored in `%1`.

---

```llvm
call ccc  void  @printint(i32  %1)
```

Pass the result (`%1`) to the `printint` function in our standard library. 

---

```llvm
ret i32 0
```

Return the value `0` to show the program successfully completed.

---

```llvm
}
```

Nice closing bracket. Time for a rest.

---

![Horse break](/images/horse2.jpg "Horse break")

## Generating IR from Haskell

Adding functions and variables means that our IR generation also becomes
stateful. We're going to define a couple more types:

```haskell
data OutputState = OutputState
  { osFunctions :: Map FunctionName LLVM.Operand,
    osVars :: Map Identifier LLVM.Operand
  }
```

It also means something can go wrong:

```haskell
data OutputError
  = CantFindVar Identifier
  | CantFindFunction FunctionName
  | NonFunctionType (Type ())
  deriving stock (Eq, Ord, Show)
```

Both these errors _shouldn't_ happen if the typechecking is working, however
it's nice to capture them properly rather than just throwing with `error`.

It means we also need a similar set of functions for adding and looking up
functions and variables, which can be found
[here](https://github.com/danieljharvey/llvm-calc/blob/trunk/llvm-calc3/src/Calc/Compile/ToLLVM.hs#L38) and are hopefully unsurprising.

A lot of our implementation are various "turn things into LLVM" functions:

```haskell
typeToLLVM :: Type ann -> LLVM.Type
typeToLLVM (TPrim _ TBool) = LLVM.i1
typeToLLVM (TPrim _ TInt) = LLVM.i32
typeToLLVM (TFunction _ tyArgs tyRet) =
  LLVM.FunctionType (typeToLLVM tyRet) (typeToLLVM <$> tyArgs) False
```

```haskell
functionNameToLLVM :: FunctionName -> LLVM.Name
functionNameToLLVM (FunctionName fnName) =
  LLVM.Name (fromString (T.unpack fnName))
```

```haskell
functionArgToLLVM ::
  (ArgumentName, Type (Type ann)) ->
  (LLVM.Type, LLVM.ParameterName)
functionArgToLLVM (ArgumentName argName, ty) =
  let llvmType = typeToLLVM (getOuterTypeAnnotation ty)
      paramName = LLVM.ParameterName (fromString (T.unpack argName))
   in (llvmType, paramName)
```

Now we can create IR for each function in our module. We put each function
variable in `State` so that we can look them up when generating expressions:

```haskell
functionToLLVM ::
  ( LLVM.MonadModuleBuilder m,
    MonadFix m,
    MonadState OutputState m,
    MonadError OutputError m
  ) =>
  Function (Type ann) ->
  m ()
functionToLLVM (Function {fnAnn, fnFunctionName, fnBody, fnArgs}) = do
  -- get llvm type of function
  retType <- case fnAnn of
    TFunction _ _ tyRet -> pure $ typeToLLVM tyRet
    _ -> throwError (NonFunctionType (fnAnn $> ()))

  let argTypes = functionArgToLLVM <$> fnArgs
      functionName = functionNameToLLVM fnFunctionName

  -- create the LLVM function
  llvmFunction <- LLVM.function functionName argTypes retType $ \args -> do
    -- save the args in the environment
    saveArgs
      ( M.fromList $
          zipWith
            ( \(ArgumentName argName, _) arg ->
                (Identifier argName, arg)
            )
            fnArgs
            args
      )

    -- build the LLVM AST for our expression
    ourExpression <- exprToLLVM fnBody

    -- return result
    LLVM.ret ourExpression
  
  -- save reference to this function in our State to lookup in other
  -- expressions
  saveFunction fnFunctionName llvmFunction
```

We have two new cases in `exprToLLVM`, for `EVar` and `EApply`. These somewhat
echo the implementation in the typechecker above. For `EVar`, we lookup the
LLVM IR in our `State`.

```haskell
exprToLLVM (EVar _ var) =
  lookupArg var
```

And for `EApply`, we lookup the function in `State`, then use LLVM's `call` to
pass all the arguments to it.

```haskell
exprToLLVM (EApply _ fnName args) = do
  irFunc <- lookupFunction fnName
  irArgs <- traverse exprToLLVM args
  LLVM.call irFunc ((,[]) <$> irArgs)
```

Finally, we bring it all together in the top level `moduleToLLVM` function,
which takes our typechecked `Module` and creates an LLVM module.

```haskell
-- | given our `Module` type, turn it into an LLVM module
moduleToLLVM :: Module (Type ann) -> Either OutputError LLVM.Module
moduleToLLVM (Module {mdExpr = expr, mdFunctions}) =
  flip evalStateT (OutputState mempty mempty) $ LLVM.buildModuleT "example" $ do
    -- get the printing function for our `expr`'s return type
    printFn <- printFunction (getOuterAnnotation expr)

    -- create all our functions
    traverse_ functionToLLVM mdFunctions

    -- create a function called `main` that will be the entry point to our
    -- program
    LLVM.function "main" [] LLVM.i32 $ \_ -> do
      -- build the LLVM AST for our expression
      ourExpression <- exprToLLVM expr

      -- print our result to stdout
      _ <- LLVM.call printFn [(ourExpression, [])]

      -- return success exit code of `0`
      LLVM.ret (LLVM.int32 0)
```

You can see all of the LLVM generation code
[here](https://github.com/danieljharvey/llvm-calc/blob/trunk/llvm-calc3/src/Calc/Compile/ToLLVM.hs).

---

### Well, well, well, if it's not the end of the article 

Congratulations, you are all functional programming language implementation experts now. Hopefully you found that somewhat interesting and/or useful. 
Next time we'll be adding basic product types and pattern matching. Great!

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[llvm reference](https://llvm.org/docs/LangRef.html)

[llvm-calc](https://github.com/danieljharvey/llvm-calc)

