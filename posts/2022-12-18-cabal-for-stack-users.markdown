---
title: Cabal for Stack users 
tags: haskell, stack, cabal 
---

`Cabal` and `Stack` are two tools for building Haskell projects. For a while, there
was only `Cabal`, and it has problems, and so `Stack`, a layer on top of `Cabal`, was
created to solve them. Perhaps embarrassed by this state of affairs, `Cabal` has
since improved, and now we have two great ways of building Haskell projects
(there is also `Nix`, which may very well be great, but I've never gotten it to
work on any non-trivial project).

When I started using Haskell, I tried both and `Stack` was the one that worked,
so a devoted `Stack` user I became.  Recently though, I've been using `Cabal` at work 
and found that learning how to talk directly to `Cabal` makes a lot of non-trivial tasks a
lot simpler than teaching `Stack` how to tell `Cabal` what to do (often via
`hpack`).

Therefore, here is a list of things I have had to translate, in case they are
helpful. This is by no means comprehensive (or even best practice) and I would
welcome any additions or corrections as I am very much still figuring this all
out.

> 💡 __What is `cabal new-build`?__
>
> For a while you needed to write `v2-` or `new-` before every `Cabal` command to tell `Cabal` not to fuck it up.
> Now, fortunately, it's generally agreed that fucking things up is bad, so
> `Cabal` chooses not to fuck things up by default, which is great.
> 
> Anyway, if you see these commands in the wild now, they all mean the same thing:
> 
> - `cabal v2-thing`
> - `cabal new-thing`
> - `cabal thing`





> 💡 __What _things_ are we building anyway?__
> 
> In both `Cabal` and `Stack` projects we split our code up into separate
> things.
> 
> - `library`: some Haskell code that exports one or more modules for use in
>   other _things_
> - `executable`: some Haskell code with a `main` function of type `IO ()` that
>   can be turned into a binary and run
> - `test suite`: some Haskell code with a `main` function of type `IO ()` that
>   returns success or failure using the exit code (`0` for success, non-zero
>   for failure)
> - `benchmark suite`: some Haskell code with a `main` function of type `IO ()`
>   that can be run to measure performance
>
> The most important thing to understand is that only code from a `library` can
> be imported into other _things_, so therefore any test that you want to test
> or benchmark should not live in the `executable`.


> 💡 __Talking about what _things_ to build__
>
> When we say `cabal build mimsa:exe:server` we mean "build the
> `server` executable in the `mimsa` cabal file. This will also build any
> libraries that the `server` depends on.
> 
> When we say `cabal build mimsa:test:mimsa-tests` we means "build the test
> suite named `mimsa-tests` in the `mimsa` cabal file. This will also build any
> libraries that the `mimsa-tests` depend on.
> 
> If there aren't many _things_ in your `Cabal` file then you can often tell
> `Cabal` less and it will do the right thing.

### .gitignore / caching

You probably want to add these to your `.gitignore` folder and consider caching
them in CI.

Stack leaves all it's local junk in a folder called `.stack-work` and all it's
global builds in `~/.stack`.

Cabal leaves all it's local junk in a folder called `dist-newstyle` and all
it's global stuff in `~/.cabal`.


### Build a _thing_ 

(see "What are we building anyway?" for a hand wavy description of a `<thing>`)

Stack: `stack build <thing>`

Cabal: `cabal build <thing>`

### Find my compiled binary

Find out where compiled binaries went:

$(stack path --local-install-root --system-ghc)/bin"

$(cabal list-bin mimsa:exe:mimsa-server)"


Find out 'installed' binaries went:

stack:
~/.local/bin

cabal:
~/.cabal/bin



Local builds:
.stack-work

dist-newstyle



stack build -> cabal build

stack bench -> cabal bench


? -> cabal update


ghcid

ghcid -c "stack repl --test --bench" -l=hlint

ghcid -c "cabal repl mimsa:exe:mimsa-server" -l=hlint



build

stack build --test --no-run-tests --bench --no-run-benchmarks

cabal update
cabal build all


install

stack install --test --no-run-tests --bench --no-run-benchmarks

cabal update
cabal install mimsa:exe:mimsa --overwrite-policy=always

test-watch


stack test --file-watch --bench --no-run-benchmarks
ghcid -c "cabal repl mimsa:tests" -l=hlint --test="main"



test
stack test --bench --no-run-benchmarks
cabal update
	cabal run mimsa:test:mimsa-test

bench

stack bench --test --no-run-tests

cabal update
cabal bench




freezing

stack : package sets
cabal : freeze files




### That's all folks 

I have probably missed a lot of things, please feel free to [yell at me about
this](/contact.html) if it makes you feel better.

Further reading:

[cabal user guide](https://cabal.readthedocs.io/en/3.4/index.html)

[stack user guide](https://docs.haskellstack.org/en/stable/)

