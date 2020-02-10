---
title: Contract testing with Quickcheck
tags: purescript, haskell, testing
---

Hello. I hope you are well. Over the last few months I have been trying out a
method for generating `Contract Tests` between services using `Arbitrary`
instances from Quickcheck which I thought it might be good to share. It's not particularly clever, which is
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

Essentially, a contract between two services is a complete set of these for each endpoint.

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

`ToJSON` is provided by `Aeson`, the excellent package for all dealings with JSON, and deserves a whole post of it's own.

The special sauce for all of this action is the `generate` function from `Test.QuickCheck.Gen`, which generates any number of values for a given `Arbitrary` instance:

```haskell
-- this will generate 100 instances of any given arbitrary value
getResponses :: (Arbitrary a) => IO [a]
getResponses = generate $ vector 100
```

We use this with a bunch of other housekeeping functions to take these 100 items and turn them into 100 files.

```haskell
-- turn a list of anything into a list of tuples with an index in the first
-- position
-- ['A', 'B'] -> [(1, 'A'), (2, 'B')]
indexList :: [a] -> [(Int, a)]
indexList as =
  List.zip [1 ..] as

-- take a root path and a file number and return a file path
createPath :: String -> Int -> String
createPath path index =
  "./" <> path <> "/" <> (show index) <> ".json"

-- this will generate 100 instances of any given arbitrary value
getResponses :: (Arbitrary a) => Proxy a -> IO [a]
getResponses _ = generate $ vector 100

-- this will turn a pile of responses into a pile of JSON responses
jsonifyList :: (ToJSON a) => [a] -> [BS.ByteString]
jsonifyList = fmap encode

-- create our responses, turn them to JSON, add numbers for file naming
listToJSON :: (ToJSON a) => [a] -> [(Int, BS.ByteString)]
listToJSON = (indexList . jsonifyList)

-- combine these functions
-- Note the `Proxy` which we use to pass the type around
responsesToJSON ::
  (ToJSON a, Arbitrary a) =>
  Proxy a ->
  IO [(Int, BS.ByteString)]
responsesToJSON arbType = listToJSON <$> (getResponses arbType)

-- save a json file using the path, number and JSON bytestring
saveFile ::
  String ->
  (Int, BS.ByteString) ->
  IO ()
saveFile path (index, json) =
  BS.writeFile (createPath path index) json

-- generate 100 APIResponse values and save them in the srcPath folder
contractWrite ::
  (ToJSON a, Arbitrary a) =>
  Proxy a ->
  String ->
  IO ()
contractWrite arbType srcPath = do
  responses <- responsesToJSON arbType
  mapM_ (saveFile srcPath) responses

-- example of using the Proxy to pass our APIResponse type to contractWrite
contractWriteAPIResponses :: String -> IO ()
contractWriteAPIResponses srcPath =
  contractWrite (Proxy :: Proxy APIResponse) srcPath
```

There is a lot of code here, but only because it has been broken down for clarity.

Example usage would be `contractWriteAPIResponses "sample"`, which would create files called `1.json`, `2.json` (up to `100.json`) in the `sample` folder in the current working directory. Note we have used a `Proxy` type here to pass the type around. An alternative way to do this could be a mixture of `TypeApplications` and `ScopedTypeVariables` but in the spirit of #simplehaskell we'll avoid them.

### Reading the sample requests

Assuming we now have a path that points to a folder full of 100 json files, we can write code to attempt to read them. We are reusing the `createPath` function from above, but other than, this should do it.

```haskell
-- read the file from the path and index provided, and try to decode it
testFile :: FromJSON a => String -> Int -> IO (Maybe a)
testFile path i = do
  str <- BS.readFile (createPath path i)
  case eitherDecode str of
    Left e -> putStrLn (show e) >>= \_ -> pure Nothing
    Right b -> pure (Just b)

-- try to decode 1.json, 2.json from the given path
-- will return only the successful results
contractRead :: FromJSON a => String -> IO [a]
contractRead srcPath = do
  maybeFound <- mapM (testFile srcPath) [1 .. 100]
  pure $ catMaybes maybeFound
```

`contractRead "sample"` will attempt to read 100 numbered `.json` files in the `sample` folder.

### That's all great but my frontend isn't written in Haskell

How to generate in Typescript using io-ts
