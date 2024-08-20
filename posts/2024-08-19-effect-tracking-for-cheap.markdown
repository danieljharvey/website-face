---
title: Effect tracking for cheap 
tags: plt, typechecker, wasm 
---

Recently I've been working on a tutorial / toy compiler that targets WASM, and
found myself in an enjoyable yak shave that I would like to share.

It goes something like this:

- We'd like an allocator so we can free our memory instead of just allocating
  it
- That allocator should be written in WASM
- Hand writing WASM is pretty stinky
- Hang on, we've got a language that compiles to WASM right here
- It would be easier to write this allocator with some inline tests
- But shit, we need to make sure those tests don't call external functions cos
  we don't want to bother making that work with the interpreter from the wasm
  package.
- Oh shit, this works, except our allocator now has an allocator in it.
- How do we track usage of an allocator...

Anyway, so yes.

We want to track:

- allocating memory, so we can only include an allocator in our WASM if our
  program needs one
- whether our tests try to use external functions or not (or whether any
  functions they call do either)
- and fuck it, since we're starting, let's see which functions mutate global
  variables, since we have those and feel kinda guilty about that.

But also, we want it to be easy, because we're lazy, this language is
supposed to be pretty minimal, and we don't have any ADTs or anything to start
wrapping things in monads.

## Compromises

1. We only track per-function, no need for more granularity than that. 

That means we go through the expression in the function and record if we see
certain things, such as a new tuple, and check any function applications
against the list of imported function names.

This is pretty much the whole implementation of that:

```haskell
abilityExpr ::
  ( MonadState (ModuleAbilities ann) m,
    MonadReader AbilityEnv m,
    MonadWriter (S.Set (Ability ann)) m,
    Ord ann
  ) =>
  Expr ann ->
  m ()
abilityExpr (ESet ann ident value) = do
  tell (S.singleton $ MutateGlobal ann ident)
  abilityExpr value
abilityExpr (ETuple ann a b) = do
  tell (S.singleton $ AllocateMemory ann)
  abilityExpr a
  traverse abilityExpr b
abilityExpr (EBox ann a) = do
  tell (S.singleton $ AllocateMemory ann)
  abilityExpr a
abilityExpr (EApply ann fn args) = do
  isImport <- asks (S.member fn . aeImportNames)
  if isImport
    then tell (S.singleton $ CallImportedFunction ann fn)
    else do
      -- whatever abilities this function uses, we now use
      functionAbilities <- lookupFunctionAbilities fn
      tell functionAbilities
  traverse abilityExpr args
abilityExpr other = bindExpr abilityExpr other
```

[bindExpr](https://github.com/danieljharvey/wasm-calc/blob/trunk/wasm-calc9/src/Calc/ExprUtils.hs#L55) is a function that lets us run a function over all the
subexpressions of `Expr`. It's a cheap version of recursion schemes.

2. Instead of tracking all the effects a function uses, we list the ones it's
   not allowed to use.

... this is easy!

## What's next?

I was supposed to write the actual `wasm-calc` tutorials. I will, I promise.

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[wasm-calc](https://github.com/danieljharvey/wasm-calc)
