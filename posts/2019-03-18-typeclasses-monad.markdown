---
title: Typeclasses - Monad
tags: haskell, typeclasses
---

The `Monad` is one of the most infamous things around Haskell, and indeed functional programming, and so writing tutorials around them has become something of a cliche. Let me be clear - I really did try and avoid writing one, but it's gotten to the point that it's difficult to talk about the other more interesting stuff without at least mentioning it.

To try and avoid falling into the regular pitfalls, we are instead going to define the simplest possible `Monad`, and then look at some examples of them in action. Hopefully you'll see that `Monads` aren't that complicated or interesting - it's actually the properties of the different datatypes that provide all the different behaviours and make it interesting.

### Id

So. This is `Id`.

```haskell
newtype Id a
  = Id { getId :: a}
```

It is a container that contains any value that we give it.

```haskell
val :: Id Int
val = Id 7
```

Which we can take out at any time with `getId`.

```haskell
plainVal :: Int
plainVal = getId val
-- plainVal = 7
```

Every `Monad` must first be a `Functor`, so let's define that for `Id`.

```haskell
instance Functor Id where
  fmap f (Id a) = Id (f a)

doubled :: Id Int
doubled = fmap (\*2) val
-- doubled == Id 14
```

It must also be an `Applicative`.

```haskell
instance Applicative Id where
  pure = Id
  (Id f) <*> (Id a) = Id (f a)

idValue :: Id String
idValue = pure "Hello!"
-- idValue = Id "Hello!"

getLength :: Id Int
getLength = Id length <\*> Id "Dogs"
-- getLength == Id 4
```

Great stuff. Now we've got `Functor` and `Applicative`, we only need one more function to make a `Monad` instance, and thats `bind` (or `>>=`). `Bind` takes an `Id a` value, and a function of signature `(a -> Id b)` that takes the value inside, does something to it, and adds another layer of `Id`. It then removes the extra layer, leaving us with `Id b`. In some places it's called `flatMap`, as it maps over the value and then flattens it.

For `Id`, it takes the value out, and then runs the function on it.

```haskell
instance Monad Id where
  (Id m) >>= k = k m
```

Pretty dull. Let's plop a few `bind` functions together. We're using Do Notation, which is a way of making using `Monad` values easier.

```haskell
doubleAndWrap :: Int -> Id Int
doubleAndWrap i
  = pure (i \* 2)

-- doubleAndWrap 1 = Id 2

doubleAFewTimes :: Int -> Id Int
doubleAFewTimes i = do
  j <- doubleAndWrap i
  k <- doubleAndWrap j
  l <- doubleAndWrap k
  doubleAndWrap l

-- doubleAFewTimes 10 = 160
```

OK! Not very interesting to be honest. `Id` doesn't really _do_ anything other than make adding numbers up more confusing. Let's look at other `Monad` instances and try and see what they do.

### Maybe

We're going to use the `Maybe` monad to chain together a `head`-type function that returns the first item _if_ it's there.

```haskell
firstItem :: [a] -> Maybe a
firstItem [] = Nothing
firstItem (a:\_) = Just a

head3 :: [[[a]]] -> Maybe a
head3 aaas = do
aas <- firstItem aaas
as <- firstItem aas
a <- firstItem as
pure a

-- head3 []          -- Nothing
-- head3 [[[1,2,3]]] -- Just 1
```

Note that as soon as we get a `Nothing`, the calculation stops, as such.

### Either

We're going to use `Either` to do some string validation. Here is our error type:

```haskell
data Error
  = TooLong
  | ContainsHorse
  | IsEmpty
  deriving (Show, Eq, Ord)
```

And here are a series of string validation functions that all return either `Right String` if the string is OK, or `Left Error` if not.

```haskell
isEmpty :: String -> Either Error String
isEmpty s
  = if null s
  then Left IsEmpty
  else Right s

tooLong :: String -> Either Error String
tooLong s
  = if length s > 10
  then Left TooLong
  else Right s

containsHorse :: String -> Either Error String
containsHorse s
  = if "horse" `isInfixOf` s
  then Left ContainsHorse
  else Right s
```

This `validate` function chains all the validations, returning either the `String` or the first `Error`.

```haskell
validate :: String -> Either Error String
validate s = do
  t <- isEmpty s
  u <- tooLong t
  containsHorse u

-- validate ""                   == Left IsEmpty
-- validate "bah horse"          == Left ContainsHorse
-- validate "really long string" == Left TooLong
-- validate "Hello"              == Right "Hello"
```

### List

The `List` monad is interesting because the flattening effect of `bind` means functions that turn values into more lists get flattened into one big list.

```haskell
moreList :: Int -> [Int]
moreList a = [a - 1, a, a + 1]
-- moreList 1 == [0, 1, 2]

lotsMoreList :: Int -> [Int]
lotsMoreList a = do
  b <- moreList a
  moreList b
-- lotsMoreList 1 == [-1,0,1,0,1,2,1,2,3]
```

### Reader

The `Reader` monad is used to pass configuration around a program. Here we will define a type for `Config` and a value for it.

```haskell
data Config
  = Config { ipAddress :: String
           , name :: String
           }

config :: Config
config = Config { ipAddress = "127.0.0.1"
                , name = "localhost"
                }
```

These functions use the `Config` value in the `Reader` to make strings, using the `ask` function to access the config.

```haskell
printName :: Reader Config String
printName = do
  config <- ask
  pure ("the name is " <> name config)

printIp :: Reader Config String
printIp = do
  config <- ask
  pure ("The ip address is " <> ipAddress config)
```

The `configReader` function shows how we can combine different `Reader` instances with `bind`

```haskell
configReader :: Reader Config String
configReader = do
  ip <- printIp
  name <- printName
  pure (ip <> ", " <> name)

withConfig :: String
withConfig = runReader configReader config
-- withConfig == "The ip address is 127.0.0.1, the name is localhost"
```

### Writer

The `Writer` monad accumulates a log as it does computations, by using the `tell` function to add items.

```haskell
addOne :: Int -> Writer String Int
addOne i = do
  tell "Add one "
  pure (i + 1)

timesTwo :: Int -> Writer String Int
timesTwo i = do
  tell "times two "
  pure (i \* 2)

maths :: Int -> Writer String Int
maths i = do
  j <- addOne i
  k <- timesTwo j
  pure k
-- runWriter maths 10 == (22, "Add one times two")
```

### IO

The `IO` monad is a very interesting one. Each time we run `bind`, the value that comes out can be affected by stuff like user input, system time, or other side effects. This is how we can write effectful code in a relatively safe manner. This is how we write code like this that actually interacts with users.

```haskell
things :: IO Unit
things = do
  \_ <- putStrLn "Hello! What is your name?"
  name <- readLn
  putStrLn ("Hello, " ++ name)
```

That is all. I feel this somewhat rushes things a lot, and there is a lot to chew on, but hopefully these examples will help you build up an intuition for how these work.

Further Reading:

[You've Been Using Monads](https://egghead.io/lessons/javascript-you-ve-been-using-monads)

[More](https://www.google.com/search?q=monad+tutorial)
