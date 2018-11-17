---
title: Working out Applicative
---

In my learning of Haskell and all the associated mathematical stuff attached to it, one of the toughest cookies to crack has been the Applicative Functor.

Coming from a background of fairly functional Javascript, functor is a very easy sell - it's `array.map`. Easy. If there's a thing in the box, do something, if there's nothing in the box, don't explode. Great!

Monad too, once you wipe away many of the somewhat terrifying analogies, isn't too brutal. Given a list `[1,2,3]` and a function that returns another list:

```haskell
moreList :: Int -> [Int]
moreList a = [a -1, a, a + 1]
```

If I `fmap` over this list like in Functor-land...

```haskell
listOfLists :: [[Int]]
listOfLists = fmap moreList [1,2,3]
-- listOfLists = [[0,1,2], [1,2,3], [2,3,4]]
```

Then I get a list of three lists. But if I use `bind` from Monad-land...

```haskell
bigList :: [Int]
bigList = [1,2,3] >>= moreList
-- bigList = [0,1,2,1,2,3,2,3,4]
```

One layer of numbers! Seems trivial in the context of lists, but if you chain together a load of functions that return a `Maybe` of some kind it's pretty great not to end up with `Maybe (Maybe (Maybe (Maybe Bool)))` at the end of it all.

So where does this leave Applicative Functor?

If we stay in the domain of lists, it means I could do example something like this:

```haskell
minusOne :: Int -> Int
minusOne i = i - 1

doNothing :: Int -> Int
doNothing = id

plusOne :: Int -> Int
plusOne i = i + 1

applicativeList :: [Int]
applicativeList = [minusOne, doNothing, plusOne] <*> [1,2,3]
-- applicativeList == [0,1,2,1,2,3,2,3,4]
```

The applicative `apply` (or `<*>`) operation takes a wrapped function and applies it a wrapped value, so in the context of lists we can use it to do the same thing Monad did, albeit in a slightly messier fashion.

This example is a bit contrived, and I suppose it's because I don't really tend to carry my functions around like this.

So my question is - what does Applicative get us that we don't get from Monad? Why not just all out every time?

I am told the difference is 'combining side effects', and I wish to understand what this really means. Let's try an example.

```haskell
data CalcFace a = CalcFace [String] a deriving (Eq)
```

```haskell
instance (Show a) => Show (CalcFace a) where
    show (CalcFace names a) =
        concat parts ++ " equals " ++ show a where
        parts = intersperse " " names

instance Functor CalcFace where
   fmap f (CalcFace names a) = CalcFace names $ f a

one :: CalcFace Int
one = CalcFace ["1"] 1

two :: CalcFace Int
two = fmap (+1) one

addOne :: CalcFace (Int -> Int)
addOne = CalcFace ["add 1"] (+1)

instance Applicative CalcFace where
    (CalcFace operation f) <*> (CalcFace value a) =
        CalcFace newNames $ f a where
            newNames = value <> operation
    pure = CalcFace []

oneAddOne :: CalcFace Int
oneAddOne = addOne <*> one

oneAddOneAddOne :: CalcFace Int
oneAddOneAddOne = addOne <*> oneAddOne

instance Monad CalcFace where
    (CalcFace _ a) >>= k = k a

addThreeMonadically :: Int -> CalcFace Int
addThreeMonadically i = CalcFace ["add 3"] $ i + 3

oneAddThreeMonadically :: CalcFace Int
oneAddThreeMonadically = one >>= addThreeMonadically

oneAddThreeAddThreeMonadically :: CalcFace Int
oneAddThreeAddThreeMonadically = one >>= addThreeMonadically
                                     >>= addThreeMonadically

```
