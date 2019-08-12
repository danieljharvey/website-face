---
title: Contract testing with Quickcheck
tags: purescript, haskell, testing
---

Hello. I hope you are well. Over the last few months I have been trying out a
method for generating `Contract Tests` between services using `Arbitrary`
instances frQuickcheck which
I thought it might be good to share. It's not particularly clever, which is
partially what I like about it, and as a result I may not have been the first
to come up with it. If I have therefore somewhat stolen your thunder please
accept my apologies in advance.

### What is Contract Testing anyway?

Contract Testing is a way of checking that two services that are going to
communicate agree on what is going to be sent and received between the two.
There is a good intro to the concept on the [Pact
website](https://docs.pact.io/) - which is itself a contract testing tool. It
is a good tool - HOWEVER it takes a lot of work and I am lazy so there what I
am going to describe is hopefully less work.

For example, a frontend says "I am going to call `/users/horse/1000` - is that cool?"
and the contract test confirms that the backend is indeed "cool" with that.

### What is Quickcheck?

Quickcheck is a Haskell library that has since been ported to many other
languages, that does a thing called Property Testing. A nice intro lives here
at [School of
Haskell](https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing) - in short it lets us test properties about things by randomly generating examples and seeing if they work.

### So, what is your idea then?

OK. So one of the main concepts behind QuickCheck is the `Arbitrary` typeclass.
Any datatype with an instance of this typeclass is able to generate random
example values that can be used for testing.

```haskell
data Horse
  = BigHorse
  | SmallHorse

instance Arbitrary Horse where
  arbitrary 
    = oneOf [ pure BigHorse
            , pure SmallHorse
            ]
```

This datatype `Horse` describes the two kinds of horse, `BigHorse` and `SmallHorse`. As is hopefully hand-wavingly evident, when the `arbitrary` function is run it returns one of `BigHorse` or `SmallHorse`.

If we can make one `Horse`, then surely we can make a load of them? Indeed!
This is what the instance for `List` look like - the `Arbitrary a =>`
constraint means that if we can make any `Arbitrary` value, we can make a list
of them.

```haskell
instance Arbitrary a => Arbitrary [a] where
  ...
```

The Quickcheck library defines instances for most basic types, as well as most
collections, therefore it is quite simple to build `Arbitrary` instances for
our request and response datatypes - and that is exactly what we're going to
do.

### An example

Let's think about a nice simple API. It receives `POST` requests in `JSON` format that translate into a datatype that looks like this:

```haskell
data APIRequest
  = APIRequest
      { name      :: String
      , age       :: Int
      , horseSize :: Horse 
      }
    deriving (Generic, Arbitrary, FromJSON)
```

Assuming that the requests are OK, it returns a response shaped like this
(again, sent over the wire in `JSON` format).

```haskell
data APIResponse
  = APIResponse
      { weight :: Int
      , goodHorse :: Bool
      }
    deriving (Generic, Arbitrary, ToJSON)
```

Note that in both cases we have derived an instance of both `Generic` and `Arbitrary` - this means we don't need to work out how to generate `arbitrary` values - so long as the types inside have their own `Arbitrary` instances.

These types are shown in `Haskell`, but the equivalent pair will also exist in
the `Purescript` frontend, so I won't duplicate them here. 

Our testing is going to work like this:

1. Our frontend will generate 100 example `APIRequest` values
2. Each one will be turned into JSON
3. Each piece of JSON is saved into a file
4. Our backend will decode each piece of JSON and see if it makes sense
5. If it does - great!

Then, for responses, we do the same thing in reverse:

1. Our backend will generate 100 example `APIResponse` values
2. Each one will be turned into JSON
3. Each piece of JSON is saved into a file
4. Our frontend will decode each piece of JSON and see if it makes sense
5. If it does - great!

Essentially, a contract between two services is a complete set of these.
