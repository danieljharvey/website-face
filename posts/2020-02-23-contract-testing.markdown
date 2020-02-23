---
title: Contract testing with Quickcheck
tags: purescript, haskell, testing
---

Hello. I hope you are well. Over the last few months I have been trying out a
method for generating `Contract Tests` between services using `Arbitrary`
instances from [Quickcheck](https://hackage.haskell.org/package/QuickCheck) which I thought it might be good to share. It's not particularly clever, which is
partially what I like about it, and as a result I may not have been the first
to come up with it. If I have therefore somewhat stolen your thunder please
accept my apologies in advance and then maybe do some reading around SEO.

### What is Contract Testing anyway?

Contract Testing is a way of checking that two services that are going to
communicate agree on what is going to be sent and received between the two.
There is a good intro to the concept on the [Pact
website](https://docs.pact.io/) - which is itself a contract testing tool. It
is a good tool - HOWEVER it takes a lot of work and I am lazy so there what I
am going to describe is hopefully less work.

For example, a frontend says "I am going to call `/users/horse/1000` - is that cool?"
and the contract test confirms that the backend is indeed "cool" with that,
shows what kind of thing it would return from that call, to which the frontend
confirms it is also "cool" with that.

![This is the agreement between the services probably looked
like.](/images/contract-handshake.png "This is what the agreement between the services
probably looked like.")

### What is Quickcheck?

Quickcheck is a Haskell library that does a thing called Property Testing. It has since been ported to many other languages. A nice intro lives here
at [School of
Haskell](https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing) - the tldr is that it lets us test properties about our programs by randomly generating examples and seeing if they fit rules that we define.

### So, what is your so-called idea then?

OK. So one of the main concepts behind QuickCheck is the `Arbitrary` typeclass.
Any datatype with an instance of this typeclass is able to generate random
example values that can be used for testing.

```haskell
module ContractTests where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck.Arbitrary

data Horse
  = BigHorse
  | SmallHorse
  deriving (Generic, FromJSON)

instance Arbitrary Horse where
  arbitrary =
    oneof
      [ pure BigHorse,
        pure SmallHorse
      ]
```

This datatype `Horse` describes the two kinds of horse, `BigHorse` and `SmallHorse`. As is hopefully hand-wavingly evident, when the `arbitrary` function is run it returns one of `BigHorse` or `SmallHorse`.

![This is a horse.](/images/contract-horse.png "This is a horse.")

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
```

Assuming that the requests are OK, it returns a response shaped like this
(again, sent over the wire in `JSON` format).

```haskell
data APIResponse
  = APIResponse
      { weight :: Int
      , goodHorse :: Bool
      }
```

These types are shown in `Haskell`, but the equivalent pair will also exist in
the frontend, and it's the compatibility between the two pairs that we will be checking.

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

Essentially, a contract between two services is a complete set of these for each endpoint. In this article I will explain the `Haskell` part of this, and will follow with the front end portion in the next one.

![These are all horses.](/images/contract-horses.png "These are all horses.")

### Creating the sample responses

We're going to need to add some typeclass instances for our `APIResponse` type first, so let's change it to the following:

```haskell
import Test.QuickCheck.Arbitrary.Generic

data APIResponse
  = APIResponse
      { weight :: Int,
        goodHorse :: Bool
      }
  deriving (Generic, ToJSON)

instance Arbitrary APIResponse where
  arbitrary = genericArbitrary
```

`genericArbitrary` is provided by the `generic-arbitrary` package which allows `Arbitrary` instances to be created for any datatype with a `Generic` instance. (For more intro on the idea of generics, the [Hackage](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html) page is a good start.)

`ToJSON` is provided by `Aeson`, the excellent package for all dealings with JSON, and deserves a whole post of it's own. For our purposes, all we need to know is that for any datatype with a `Generic` instance, we can derive a free typeclass for turning it to and from JSON.

The special sauce for all of this action is the `generate` function from
`Test.QuickCheck.Gen`, which generates any number of values for a given
`Arbitrary` instance. We use this with a bunch of other housekeeping functions to take these 100
items and turn them into 100 files. I have broken this down into a bunch of
functions so that it's hopefully easier to follow.

Firstly, a couple of helpers for adding index numbers to lists...

```haskell
indexList :: [a] -> [(Int, a)]
indexList as =
  List.zip [1 ..] as

-- indexList ['A', 'B'] == [(1, 'A'), (2, 'B')]
```

...and creating a file path using said index...

```haskell
createPath :: String -> Int -> String
createPath path index =
  "./" <> path <> "/" <> (show index) <> ".json"

-- createPath "output" 1 == "./output/1.json"
```

Next, we will make our functions for generating instances and saving them to
files. This first function uses a `Proxy` (from
[Data.Proxy](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Proxy.html))
to pass the type we would like to generate (as such). We have chosen `100` as
it is as good a number as any.

```haskell
getResponses :: (Arbitrary a) => Proxy a -> IO [a]
getResponses _ = generate $ vector 100
```

This next function takes our list of randomised values, turns them to JSON, and pops them in a `Tuple` along with an index.

```haskell
listToJSON :: (ToJSON a) => [a] -> [(Int, BS.ByteString)]
listToJSON = (indexList . jsonifyList)
  where
    jsonifyList = fmap encode
```

Finally, we put them together along with some glue code (using `writeFile` from
`Data.ByteString.Lazy`) to save the generated `JSON` files.

```haskell
contractWrite ::
  (ToJSON a, Arbitrary a) =>
  Proxy a ->
  String ->
  IO ()
contractWrite arbType srcPath = do
  let saveFile = \path (index, json) ->
        BS.writeFile (createPath path index) json
  responses <- listToJSON <$> (getResponses arbType)
  mapM_ (saveFile srcPath) responses
```

To use it with our datatype, we use a `Proxy` as such to pass it the type we
want (in our case, `APIResponse`, but the same code will work for any type with
`Arbitrary` and `ToJSON` instances)

```haskell
contractWriteAPIResponses :: String -> IO ()
contractWriteAPIResponses srcPath =
  contractWrite (Proxy :: Proxy APIResponse) srcPath
```

If we crack this open in `ghci` we can run `contractWriteAPIResponses "sample"`, and it will create files called `1.json`, `2.json` (up to `100.json`) in the `sample` folder in the current working directory.

Our frontend tests can now read these and make sure that they understand them.
But how do we make sure our backend understands the front end requests?

![I am feeling bad about how many words there
are.](/images/contract-business-man.png "I am feeling bad about how many words
there are.")

### Reading the sample requests

Assuming that our front end has also created some sample requests in a similar fashion, reading them and checking they are decodable is a simpler affair.

Given a path that points to a folder full of 100 json files, we can write code to attempt to read them. We are reusing the `createPath` function from above, but other than, this should do it.

This function takes a `Proxy` for our decoding type, a path and an index, and
tries to decode the file it finds.

```haskell
testFile :: FromJSON a => Proxy a -> String -> Int -> IO (Maybe a)
testFile _ path i = do
  str <- BS.readFile (createPath path i)
  case eitherDecode str of
    Left e -> putStrLn (show e) >>= \_ -> pure Nothing
    Right b -> pure (Just b)
```

Here we take a path to the folder full of files and attempt to read `100`
numbered `.json` files in it.

```haskell
contractRead :: FromJSON a => Proxy a -> String -> IO Int
contractRead arbType srcPath = do
  maybeFound <- mapM (testFile arbType srcPath) [1 .. 100]
  pure $ length $ catMaybes maybeFound
```

And here we put it all together using our `APIRequest` type.

```haskell
contractReadAPIRequest :: String -> IO Int
contractReadAPIRequest srcPath =
  contractRead (Proxy :: Proxy APIRequest) srcPath
```

Cracking open `ghci` and running `contractRead "sample"` will attempt to read 100 numbered `.json` files in the `sample` folder.

### Digression 1.

Note we have used a `Proxy` type here to pass the type around. An alternative way to do this could be a mixture of `TypeApplications` and `ScopedTypeVariables` but in the spirit of `#simplehaskell` we'll avoid them.

### Digression 2.

Whilst preaching simplicity, it seems enjoyably hypocritical to point out in
the same breath that we needn't have made a standalone instance of `arbitrary`
each time, and it's actually a great opportunity to crack open `DerivingVia`.

An alternative method could looks something like this, and save up to 15
characters per datatype.

```haskell
-- this newtype can derive Arbitrary via Generic, so we use Deriving Via to
-- steal it's powers!
newtype GenericArb a
  = GenericArb {getGenericArb :: a}
  deriving (Generic)

instance (Generic a, Arbitrary a) => Arbitrary (GenericArb a) where
  arbitrary = genericArbitrary

data APIRequest2
  = APIRequest2
      { name2 :: String,
        age2 :: Int,
        horseSize2 :: Horse
      }
  deriving (Generic, FromJSON)
  deriving (Arbitrary) via (GenericArb APIRequest2)

data APIResponse2
  = APIResponse2
      { weight2 :: Int,
        goodHorse2 :: Bool
      }
  deriving (Generic, ToJSON)
  deriving (Arbitrary) via (GenericArb APIResponse2)
```

### Make sense?

No. Thought not. Regardless, I'll follow up with how to great the front end
part. I'll be using `Typescript` because quite frankly if you understand this
then doing it in `Purescript` isn't wildly different.
