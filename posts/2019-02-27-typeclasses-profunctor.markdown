---
title: Typeclasses - Profunctor
tags: haskell, typeclasses
---

Hello. Let's make a box for putting functions in...

```haskell
newtype FuncBox b c
  = FuncBox { runFuncBox :: b -> c }
```

...and a function...

```haskell
length :: String -> Int
length s = foldl' (\c _ -> c+1) 0 s
```

...and then let's put a function in this box:

```haskell
length' :: FuncBox String Int
length' = FuncBox length
```

Great. A function in a box. You may be concerned that something interesting is going on here, so just to assure you that it's not, let's run the function using `runFuncBox`, to reassure ourselves that this box is doing no funny business and we're really just unwrapping that `newtype`.

```haskell
length'' :: Int
length'' = runFuncBox length' "dog"
-- length'' == 3
```

OK. Good stuff. We now have a very longwinded way of running the `length` function. Good stuff. Big day.

### The plot thickens

Now what if want to run this weird function, but instead of having a `String` to hand, we only have an `Animal`...

```haskell
data Animal = Horse | Dog | Cat
  deriving (Show)
```

I can see you there. You're getting excited and jumping straight for our old pal [Contravariant](/posts/2018-11-17-typeclasses-contravariant.markdown) aren't you. But wait! That's not an entirely terrible decision but it turns out there are other problems that we must also solve.

### Oh no

That's right. Heartbreaking, it also turns out that our somewhat contrived API can output numbers, but instead only lists full of a delicous datatype called `Egg`.

```haskell
data Egg = Egg
```

We can turn any `Int` into an `[Egg]` using this excellent function.

```haskell
repeatEgg :: Int -> [Egg]
repeatEgg s
  = replicate s Egg

threeEggs :: List Egg
threeEggs = repeatEgg 3
-- threeEggs == [Egg, Egg, Egg]
```

OK. So somehow we need to turn an `Animal` into a `List` of `Egg`. We could `contramap` over our input to turn `Animal` into `String`, run the original `length` function, and then `fmap` over the result to turn `Int` into `[Egg]`. That could work. But what if we could solve this problem using a typeclass, that would be pretty fucking snazzy wouldn't it?

### Hold on, he's only gone and done it

Looks like a job for our new friend `Profunctor` I reckon. Let's have a wee look in `ghci` and see what what the hell it's deal is.

```bash
import Data.Profunctor
Data.Profunctor> :i Profunctor
```

```haskell
class Profunctor (p :: _ -> _ -> \*) where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    lmap :: (a -> b) -> p b c -> p a c
    rmap :: (b -> c) -> p a b -> p a c
    {-# MINIMAL dimap | lmap, rmap #-}
```

Look at that! It gives a `Contravariant` with the `lmap` function, and a regular `Functor` with `rmap`, or everything smashed together in this new exciting `dimap` function. Let's look at that a little closer.

```haskell
dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
```

That's pretty beastly, let's put in some real concrete things.

```haskell
dimap
  :: (Animal -> String)   -- function to go at the start
  -> (Int -> [Egg])       -- function to go at the end
  -> FuncBox String Int   -- original container
  -> FuncBox Animal [Egg] -- exciting new container
```

import Data.Profunctor

instance Profunctor FuncBox where
dimap before after (FuncBox f)
= FuncBox (after . f . before)

data Egg = Egg
deriving (Show, Eq, Ord)

data Animal = Horse | Dog | Cat
deriving (Show)

repeatEgg :: Int -> [Egg]
repeatEgg s
= replicate s Egg

length' :: FuncBox String Int
length' = FuncBox length

leftMapped :: FuncBox Animal Int
leftMapped
= lmap show length'

rightMapped :: FuncBox String String
rightMapped
= rmap show length'

dimapped :: FuncBox Animal [Egg]
dimapped
= dimap show repeatEgg length'

one :: Int
one = runFuncBox length' "horses"
-- one == 6

two :: [Egg]
two = runFuncBox dimapped Dog
-- two == [Egg, Egg, Egg]

three :: Int
three = runFuncBox leftMapped Horse
-- three == 5

four :: String
four = runFuncBox rightMapped "Dog"
-- four == "3"
