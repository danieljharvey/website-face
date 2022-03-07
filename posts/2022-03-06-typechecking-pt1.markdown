---
title: Typechecking 
tags: plt, mimsa, typechecker 
---

So I've been getting really interested in typechecking, and well, the best way
to learn is to try and explain it in words, so here we go.

There are many kinds of typechecking, but today I want to explain a variation
on Hindley Milner (I've also seen it called Damas Milner, please feel free to
ping with the actual truth, fact fans). This kind of type system is the kind
you'll find under OCaml, Elm, Haskell and Purescript, although each language has their own variations (and in the case of Haskell, a terrifying number of extensions to it). 

## Stages of typechecking

The system we'll be looking at today uses three stages called `Elaboration`, `Unificiation` and `Substitution`. Many explanations of HM combine all these into one big stage, which is simpler in some ways, but more difficult once things get spicy. I hope these stages become apparently as we work through the example, but these
stages basically mean:

### Elaboration

Here go through the expression we'd like to typecheck, marking each
  part of the AST with the thing we think it is (and if we don't know, we sort of guess). We'll also produce
 a set of constraints about the types we've worked out (a constraint is
 something like "I don't know `a` or `b` are but I know they're the same
 thing")

### Unification

Here we go through all of the constraints and "solve" them by
  smashing them together and seeing what we learn. For instance, if we have the
  constraint "`a` and `Boolean` are the same" then we have learned "wherever we
  see `a` we should change it for a `Boolean`. However if we have the
  constraint "`Integer` and `Boolean` are the same" then we have learned that
  the expression is invalid and we should show the user a type error. The
  outcome of unification is either a type error or a set of substitutions, such
  as "replace all `c` values with `String`".

### Substitution

Assuming we had no type errors in the previous step, then
  subsitution is the process of taking all the things we have learned and
  applying them to both to expression and to the other substitutions. For
  example, if we have learned "`a` is an `Integer`" then we can change any `a`
  values in our expression to `Integer`, and also change the substitution "`c`
  is a pair of `a` and `b`" into "`c` is a pair of `Integer` and `b`".

-- Note: I've said "AST" - this means Abstract Syntax Tree - it's code,
represented as a data structure. This will hopefully make sense in due course.

## A working example

We're going to start by typechecking the simplest stuff, and then build up
features as we go.

### Literals

Literals are static values declared in code. Our toy language has the
`Boolean`, `Integer` and `String` types. As this means there is no ambiguity
between them, we can work out their types in the `Elaboration` stage alone.

![Elaborating literal values](/images/typecheck-1-elaborate-literals.png)

The values are on the left in black, the types on the right in blue. No weird surprises here, hopefully. 

### Let bindings

It is nice to be able to give things names, so we should have some let bindings
in our language. These allow us to introduce a new concept - the typechecking
environment. A let binding has two sub-expressions - the first is the one
that gets "bound" and assigned to the name, and then the body, which can refer
to the bound expression by name.

![Elaborating let binding](/images/typecheck-1-elaborate-let.png)

In this example, our environment (shown in green) is initially empty, but as we were able to
elaborate the type of `a` as `Integer`, the environment when typechecking the
body also includes `a == Integer`. However, this has no effect on the type of
the whole expression, as the body of the let binding is `True` which we know is of type `Boolean`.

### Variable

It's all well and good making variables, but it's even more useful to refer to
them later.

![Elaborating variable](/images/typecheck-1-elaborate-var.png)

When using a variable, we look it up in the environment. If it's not there, we
throw a type error and tell the user they've made a typo or something. If it
is, then we return the type from the environment.

### If / Then / Else

#### Elaboration

An if statement in our language is an expression, meaning it must return
something from both the `then` and the `else` branch. It contains three
sub-expressions, one for the predicate we are testing (which must be of type
`Boolean`), one for the `then` branch, and one for the `else` branch.

![Elaborating if / then / else expressions](/images/typecheck-1-elaborate-if.png)

Because each of these sub-expressions are literal values, we can elaborate their
values as we did above. The return type is `Integer`, as that is the type we
have worked out for the `then` branch (it could as well be the type of the
`else` branch, they should be the same).

How do we ensure all our rules work out? That's the job of the constraints that
we have generated (shown in red). We have one that ensures the type of `True` matches
`Boolean`, and another that ensures the types of `1` and `2` are the same.

For the first time, the `Elaboration` stage has created some constraints, so
we'll need to unify them.

#### Unification 

Unification in the context of typechecking is about smashing two things
together and seeing what we learn. Broadly, the following things can happen:

- The two things are the same, so everything is fine and we learn nothing new

- The two things are different and at odds with one another (ie, we try and
  unify `Integer` with `String`) - this creates an error

- One (or both) of the things are unification variables, creating a
  `Substitution`. We'll encounter this in more detail shortly.

Here the two things are the same in both cases, so everything is fine and we
learn nothing new.

![Unifying if / then / else expressions](/images/typecheck-1-unify-if.png)

### Lambda

When elaborating lambdas, we don't know what the argument will be, so instead
we'll guess it. We do this by creating a fresh unknown type variable called a
"unification variable", here we
use `unknown-1`. Each of these should be unique within the typechecking.

![Elaborating lambda](/images/typecheck-1-elaborate-lambda.png)

Then, when we elaborate the body of the lambda, we add `a == unknown-1` to the
environment.

### Function application

#### Elaboration

Given the `id` function has already been defined, let's apply a value to it.

![Elaborating function application](/images/typecheck-1-elaborate-application.png)

Since the function type is `unknown-1 -> unknown-1`, we apply `Boolean` to it,
by creating a constraint between the first `unknown-1` and `Boolean`. Then the
return type from elaboration is the second half of the function type (also
`unknown-1`).

Surely it should be `Boolean` though right? That's where the other stages we've been ignoring come into play.

#### Unification 

![Unifying function application](/images/typecheck-1-unify-application.png)

When we unify `unknown-1 == Boolean`, because `unknown-1` is a unification
variable, then it unifies with everything. This means we create one
substitution, "replace all instances of `unknown-1` with `Boolean`".

#### Substitution

Applying our substitution to the result of elaboration gives us our final type,
`Boolean`

![Substituting function application](/images/typecheck-1-substitute-application.png)



### No more words

That is the end of the words. It's a bit of a braindump, and pretty much only
for my own benefit, but I hope it is perhaps vaguely interesting. Who knows?

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[quickcheck](https://hackage.haskell.org/package/QuickCheck)

[fast-check](https://github.com/dubzzz/fast-check)

[mimsa](https://github.com/danieljharvey/mimsa)
