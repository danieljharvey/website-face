---
title: Mr Migration
tags: haskell, migratable
---

A few weeks ago I gave a talk at London Haskell that was basically an advert for a data migrating library I haven't even finished yet. However, I managed on the whole to hide this fact and some people even came up afterwards and asked me how the library was performing in production to which of course I said "very well indeed, because of course i am very good at computers" and quickly changed the subject.

What follows is basically the crap from my slides, occasionally turned from bullet lists into sentences where I realised I had overused that particular presentation device a little much.

Sitting comfortable? Then we shall begin...

### In The Beginning...

...we had server side applications.

* If the code agrees with the DB schema...

* Deploy!

* **Great!**

### Then Came Javascript...

* Suddenly all our data was spread around the place

* Things didn't necessarily agree with one another 

* There was sometimes *JQuery*.

And at some point we land at 

### The Traditional Backend / Frontend Monolith

- When the back end changes...

- Change the front end too.

- **Deploy everything together**

- (did you forget to update the DB schema?)

- **DO THAT QUICKLY**

- Forget about the past

- **YOLO**

### So...

What could possibly go wrong with this?

![A relatable picture that was not in the presentation to break things up a bit.](/images/migrate-1.png "A relatable picture that was not in the presentation to break things up a bit.")

### Problem One

`Company A` have found that DB schema changes make changing the application more difficult than they would like. Therefore they choose to use **event sourcing**. As the application changes, the DB schemas keep up, but they are soon left with an event table full of various historical versions of JSON data.

- *What is this data? Do they still understand it?*

### Problem Two

`Company B` decide their code is so good that they are going to create a **public API**. Other companies decide to use this API, and annoyingly want it not to change at random. Therefore the API owners make promises not to change their API (even though they bloody love changing their API because they are 10x hackers who just can't stop delivering business value).

- *How can they make changes to this without breaking everything?*

### Problem Three

`Company C` have noticed that *large monoliths take ages to deploy*. They would also like to decouple teams to maximise parallelisation of work. They adopt a **microservice architecture**. Suddenly, services that talk to one another aren't guaranteed to have versions or interfaces that match, so a old service can be receiving requests from a very old service that hasn't been updated.

- *How will they cope with communicating with any number of historical deployments?*

### A concrete example

Here's a data type that we use in our business critical application. It is called `OldUser`, which we never really questioned at the time.

```haskell
data OldUser
  = OldUser 
    { firstName :: String
    , surname   :: String
    , pet       :: String
    , age       :: Int
    }
```

Business is going pretty well, I can't imagine my meeting this afternoon will go badly...

### Oh no!

![A change in business requirements has been spotted.](/images/migrate-2.png "A change in business requirements has been spotted.")

### Pivot immediately

Apparently we can increase profitability by *30%* by using newtypes properly.

```haskell
newtype Name
  = Name { getName :: Text.Text }
```

And replacing `String` selections of `pet` types with a more restrictive sum type.

```haskell
data OldPet
  = OldDog
  | OldCat
  | NoPet
```

These changes took all night, but you really pulled through there.

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
```

Great job.

### Hold On Though

Business is obviously booming now, but what are we going to do about:

- Third parties that will insist on using `OldUser` in their API calls for the
  next 18 months

- Stored JSON data with the old data shape

### What options do we have?

1. As well as developing new code, we keep **old code** for dealing with the **old data**

  * This seems fine...
  * ...till you're fixing bugs in the old system too
  * Shit! More fixes! Now you're got an new-old system as well as the new-new one.
  * More fixes again! Now we have a new-old-new-old system to maintain as well as your main one, which is a new-new-new system by now? It may have been superceded too. Oh dear.

2. Or migrate the **old datatypes** to the **new datatypes** and keep one set of logic.

  * Logic stays in new code
  * Bug fixes in business logic happen once
  * Logic of migration separated from business logic

### Decision time

If you choose Option `1`, you are on your own. However, if you have chosen Option `2`, read on...

![Another picture of an angry business person.](/images/migrate-3.png "Another picture of an angry business person.")

### Functions we will need: migrate

The first thing we'll need is a function to convert our *old terrible datatype* into our *new incredible exciting datatype*, in this case `OldUser` to `NewUser`.

```haskell
migrate :: OldUser -> NewUser
```

Actually. Let's be realistic about this, and account for the idea that this operation could fail, as life is a bin.

```haskell
migrate :: OldUser -> Maybe NewUser
```

### Functions we will need: parse

We'll also need some functions for decoding JSON, as I have made the somewhat brazen assumption this is a REST API that only receives JSON payloads. We'll use functions from the `Aeson` library because it is reasonably ubiquitous.

This function will attempt converting some `JSON` into an `OldUser`

```haskell
parseOldUser :: JSON -> Maybe OldUser
parseOldUser json
  = parseMaybe (parseJSON json)
```

And this very similar function will try and convert from `JSON` into a `NewUser`.

```haskell
parseNewUser :: JSON -> Maybe NewUser
parseNewUser
  = parseMaybe . parseJSON
```

### Putting it together

We can then make a function that takes some `JSON`, and then tries to decode it into a `NewUser`. If it can't decode it into a `NewUser`, it tries to parse it into an `OldUser`, and if that succeeds, it uses some sort of `migrate` function to turn `OldUser` into a `NewUser`.

Said function looks something like this:

```haskell
parseSomeKindOfUser :: JSON -> Maybe NewUser
parseSomeKindOfUser json
   =   parseNewUser json
  <|> (parseOldUser json >>= migrate)
```

(The `<|>` operator comes from the `Alternative` typeclass, and works sort of like the `||` function. The intuition is `try this OR try this`)

### Does it scale though?

It seems to do the job, with a couple of datatypes, however it's easy to see how it could get out of hand...

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
    <|> parse d >>= migrate >>= migrate >>= migrate
    <|> parse e >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse f >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse g >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse h >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse i >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse j >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse k >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse l >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate 
    <|> parse m >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
```

### So this method is a no?

I don't think so. But writing all of that is a bit much. Hopefully right now you are asking "Why can't I get the compiler to do this for me?".

![The compiler is your friend.](/images/migrate-4.png "The compiler is your friend.")

### Good Point.

OK! Let's give it a smash. We start by making a typeclass and using it to version tag our datatype.

```haskell
class Versioned (label :: Symbol) (num :: Nat) where
  type num `VersionOf` label :: Type
```

(This `VersionOf` type here is an `associated type family` - a type level function that is scoped to only work inside the typeclass it is defined in.)

The typeclass defines a function we can use to find a datatype from the `label` and the `num`. For example, we can make a label called `"User"`, and make `OldUser` version `1` of it:

```haskell
instance Versioned "User" 1 where
  type 1 `VersionOf` "User" = OldUser
```

We'll also define an instance for `NewUser`, and make that version `2` of it.

```haskell
instance Versioned "User" 2 where
  type 2 `VersionOf` "User" = NewUser
```

### Linking versions together

Next we make a typeclass for migrations that uses our `VersionOf` type function:

```haskell
class Migratable (label :: Symbol) (num :: Nat) where
  fromPrevious
    :: (num - 1) `VersionOf` label
    -> Maybe (num `VersionOf` label)
```

It lets us define a function from the previous version of a datatype to the current one, so let's use it to migrate `OldUser` to `NewUser`.

```haskell
instance Migratable "User" 2 where
  fromPrevious :: OldUser -> Maybe NewUser
  fromPrevious older
    = Just $ NewUser 
              { newFirstName = Name (Text.pack (firstName older))
              , newSurname   = Name (Text.pack (surname older))
              , newPet       = readPet (pet older)
              , newAge       = age older
              }
    where
      readPet s
        | s == "dog" = OldDog
        | s == "cat" = OldCat
        | otherwise  = NoPet
```

### Problem: Migrating from old JSON versions

Once we've defined instances of the `Versioned` and `Migratable` typeclasses for our data, the `Data.Migratable` library functions start doing helpful things. It provides a version of `migrate` function we defined concretely earlier, but with a much more exciting (confusing) type signature.

```haskell
migrate 
  :: earliest `VersionOf` label 
  -> Maybe (target `VersionOf` label)
```

It means "I can take the `earliest` version of `label` and `Maybe` return you the `target` version of `label`". We use *type applications* to pass `versions` and a `label` to convert an old datatype to a new one.

```haskell
oldToNew :: OldUser -> Maybe NewUser
oldToNew = migrate @1 @2 @"User"
```  

Going from version `1` to `2` means we've just done a shitload of work for a single conversion, but the same function will recursively convert through as many versions of the datatype as you like:

```haskell
veryOldToVeryNew :: OldUser -> Maybe VeryNewUser
veryOldToVeryNew = migrate @1 @100 @"User"
```  

### Solution: parseJSONVia

The `Data.Migratable` library also provides us `parseJSONVia`, with the following exciting type signature:

```haskell
parseJSONVia
  :: JSON 
  -> Maybe (target `VersionOf` label)
```

We could use to try converting from both `OldUser` and `NewUser` like this:

```haskell
parseSomeKindOfUser 
  :: JSON 
  -> Maybe NewUser
parseSomeKindOfUser 
  = parseJSONVia @"User" @1 @2
```

Underneath, this is doing our `parse` and `migrate` pattern under thge good
```haskell
thing = parse @NewUser
    <|> parse @OldUser >>= migrate
```

### A note on Type Applications

To understand how to pass the types to the `parseJSONVia` function, we need to look at the `Schema` typeclass that provides this functionality:

```haskell
class Schema (label :: Symbol) (earliest :: Nat) (target :: Nat) where
  parseJSONVia :: JSON.Value -> JSON.Parser (target `VersionOf` label)
```

When we use it, we are passing it types in the order they appear in the class declaration.

```haskell
parseJSONVia @"User" @1 @2
```

Therefore, we are passing the type-level symbol `"User"` as the first argument `label`, then a type-level natural number `1` as the starting version `earliest`, and finally another type-level natural `2` as the target version `target`.

### Problem: Uniqueness checking

Uniqueness checking is another feature of `Data.Migratable`. Let's say that we have this data type `Info`:

```haskell
data Info
  = Info 
      { amount: Pounds }
```  

Then, after another hard pivot, we change the units:

```haskell
data NewInfo
  = NewInfo
      { amount :: Pennies }
```

In our static typed ivory tower, we are fine, but our clients keep sending us
  this:

```json
{ "amount": 100 }
```

What is it? 100 pennies? 100 pounds? How can we stop this confusion?

### Solution: Checking for duplicates with QuickCheck

`Data.Migratable` provides us with the `matchAll` function:

```haskell
matchAll :: IO (Either [MatchError] [Integer])
```

* It uses `QuickCheck` and it's `Arbitrary` instances to generate random `JSON`
  values for each datatype

* Then tries to load each generated value as each version of the datatype

* And tells us how many version of a datatype each generated instance is able
  to decode

* If it's one each - we're going to have a good time:

```haskell
describe "Uses Arbitrary to generate said tests" $ do
  it "Checks if our datatypes will get confused" $ do
    found <- matchAll @1 @4 @"User"
    found `shouldBe` Right [1,2,3,4]
```

* But if our `JSON` representations are non-unique, we'll know:

```haskell
describe "Our Pennies and Pounds schema" $ do
  it "Spots our problematic matching schema" $ do
    found <- matchAll @1 @2 @"Same"
    found `shouldBe` Left [Duplicates 1 [2,1], Duplicates 2 [2,1]]
```

* And we can fix our data types to ensure uniqueness.

```haskell
data Info
  = Info 
      { amountPounds :: Pounds }

data NewInfo
  = NewInfo
      { amountPennies :: Pennies }
```

- Good job.

### Problem: getting a FromJSON instance

The `Aeson` library works by making datatypes define instances of the `FromJSON` typeclass, and packages like `Servant` allow us to automagically create web servers that use these types. Can we still use all this good stuff?

### Solution: FromJSON instance around a newtype wrapper

```haskell
newtype APIUser
  = APIUser { getAPIUser :: WhateverTheNewestUserTypeIsTheseDays }
```

...and using `parseJSONVia` to create a `FromJSON` instance for that datatype...

```haskell
instance JSON.FromJSON APIUser where
  parseJSON a
    = APIUser <$> parseJSONVia @"User" @1 @4 a
```

...we can make a `Servant` server that can read any of our historical datatypes.

```haskell
type ExcellentApi =
  "user" :> Get '[JSON] [APIUser] 
```

Great job!

### Problem: Mistakes in our datatypes

What if we make "mistakes" in our types - like removing a piece of data we later decide we need? Here is a first version of some data.

```haskell
data Dog
  = Dog { name :: String
        , age  :: Int
        }

instance Versioned "Dog" 1 where
  type 1 `VersionOf` "Dog" = Dog
```

This is the second version of the datatype, where we remove `age`.

```haskell
data AgelessDog
  = AgelessDog
      { name :: String }

instance Versioned "Dog" 2 where
  type 2 `VersionOf` "Dog" = AgelessDog
```

Oops. We needed that. It's back in version `3`.

```haskell
data WithAgeDog
  = WithAgeDog
      { name :: String
      , age  :: Int
      , tail :: Bool
      }

instance Versioned "Dog" 3 where
  type 3 `VersionOf` "Dog" = WithAgeDog
```

However, this means any version `1` piece of data will convert through version `2` and lose everything on the way.

### Solution: multiple import paths

Let's change our declarations...

```haskell
data Dog
  = Dog { name :: String
        , age  :: Int
        }

instance Versioned "Dog" 1 where
  type 1 `VersionOf` "Dog" = Dog

instance Versioned "AgeDog" 1 where
  type 1 `VersionOf` "AgeDog" = Dog
```

(We've ignored the middle one for now - it is the same)

```haskell
data WithAgeDog
  = WithAgeDog
      { name :: String
      , age  :: Int
      , tail :: Bool
      }

instance Versioned "Dog" 3 where
  type 3 `VersionOf` "Dog" = WithAgeDog

instance Versioned "AgeDog" 2 where
  type 2 `VersionOf` "AgeDog" = WithAgeDog
```

Then our parsing function becomes (something like)

```haskell
parseSomeKindOfDog :: JSON -> Maybe WithAgeDog
parseSomeKindOfDog json
  =  parseJSONVia @"AgeDog" @1 @2 json
 <|> parseJSONVia @"Dog" @1 @3 json
```

How does it work? 

- First, we try the lossless path

- Failing that, we try the lossy path to pick up any `AgeLessDog` values.

### OK. Sum up what you've said and stop selling me your crappy non-existent library

So hopefully, this technique should let you:

* Define migrations outside the main logic code, throw those into a file and forget about them forever until the next migration.

* Use simple ADTs for my types if one feels like it.

* Avoid historical code making new code more complicated.

### But does it work?

Who knows? See the code at
[https://github.com/danieljharvey/migratable](https://github.com/danieljharvey/migratable) and decide for yourself. I mean, the tests pass, but what does that really tell us.

### Addendum

After the talk, somebody suggested that it's all very well accepting old JSON data version in an API, but really you'd need to provide the response in the old format too. This was a really annoyingly good point, so this is the next feature I am working on for the library - the plan so far is to create a kind of opposite of the `Migratable` typeclass for responses, that goes from newer to older versions instead.
