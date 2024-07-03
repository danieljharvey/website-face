---
title: llvm-calc is dead, long live wasm-calc 
tags: plt, mimsa, typechecker, llvm, wasm 
---

So last year I [started writing a
series](/tags/llvm.html) on compiling a functional language to
LLVM, and I have to confess, I came [utterly unstuck](https://github.com/danieljharvey/llvm-calc/pull/2).

![Failure](/images/failure.webp "Failure") 

Until recent versions, it tried to be helpful by making you specify types
for all pointers you pass around. This is really helpful in simple cases as you
get basic typechecking at compile time. However, once you start implementing things like sum types (ie, reserve a slab of memory, and stuff it with one of several sets of data), it becomes super annoying.

For example, compiling an `Either<Int, Bool>` type requires you to constantly cast between "arbitrary array type we've chosen
that takes up the right amount of memory", "this thing if it turns out
to be `Left<Int>`" and "this thing if it turns out to be `Right<Bool>`.

This has been solved though! More recent versions of LLVM have the concept of an [Opaque
pointer](https://llvm.org/docs/OpaquePointers.html), which allows us to pass
pointers around without caring what type they are, and then think about the
types when it comes to actually interacting with the underlying data. This is
ideal!

However, it's only available in LLVM 14 onwards, and getting a working
version-matched set
of LLVM Haskell libraries turns out to be precarious at best. I got [reasonably
close](https://github.com/danieljharvey/llvm-calc/tree/djh/llvm-15) using
the [LLVM-15 branch](https://github.com/llvm-hs/llvm-hs/tree/llvm-15) of `llvm-hs-pure`, but could not find an equivalent for [llvm-hs-pretty](https://github.com/llvm-hs/llvm-hs-pretty).

Basically, it was all getting a bit fiddly and stupid, and writing a tutorial that is either a) already out of date or b) relies on a set of arbitrary commits from a
bunch of forks doesn't feel like it will be particularly useful, so I have decided to change tack.

## The good news?

I decided to port the project to compile to WebAssembly instead! Development of
the thing itself has been going really well, see for yourself here: [wasm-calc](https://github.com/danieljharvey/wasm-calc)

The language is fully featured enough that we can create a very simple
[wasm-4](https://wasm4.org/)
game with it.

![Some sort of graphics created from our language](/images/wasm-4.png "Some sort of graphics created from our language")

Some thoughts so far:

- The [wasm](https://hackage.haskell.org/package/wasm) package for Haskell is lovely!

- Not being able to cheat by including a bunch of C in a runtime has forced me to learn how allocators work 

- There's a bunch of useful resources, like
      [watlings](https://github.com/EmNudge/watlings) and [Mozilla's
      docs](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format).

## What's next?

As well as serving as a big excuse, this blog is supposed to be a forcing
function to write the tutorials to go with all the `wasm-calc` versions. I'll
start by porting the `llvm-calc` blogs, and then move onto the new features
we've got.

Make sense? If not, [get in touch](/contact.html)!

Further reading:

[wasm-calc](https://github.com/danieljharvey/wasm-calc)
