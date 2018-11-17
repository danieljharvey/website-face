---
title: Typeclasses - Applicative
---

In my learning of Haskell and all the associated mathematical stuff attached to it, one of the tougher conceptual cookies to crack has been the Applicative Functor.

Coming from a background of fairly functional Javascript, functor is a very easy sell - it's `array.map`. Easy. If there's a thing in the box, do something to the thing, if there's nothing in the box, don't freak out about the lack of things or explode. Great!

```haskell
increase :: Int -> Int
increase i = i + 1

-- fmap increase [1,2,3] == [2,3,4]
```

These functions can even return lists of their own...

```haskell
moreList :: Int -> [Int]
moreList a = [a -1, a, a + 1]
```

```haskell
listOfLists :: [[Int]]
listOfLists = fmap moreList [1,2,3]
-- listOfLists = [[0,1,2], [1,2,3], [2,3,4]]
```

Trying to understand the actual point of an Applicative Functor, especially to my Javascript-addled mind, was much trickier. The first explanations I heard were something like:

#### "Oh, applicative? It's for taking a function that in a box, and a value in a box, and then combining them to make a new value which is also in a box"

Now I am all up for putting values in boxes. Bloody love it. But functions? What are you playing at? Ridiculous talk.

Could it help us with our list changing exercise? Sort of...

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

OK, so instead of a list of lists, we have one list (and one new terrifyingly meaningless infix operator, `<*>`, wonderful). Seems like a lot more work too, and we could probably just have used `concat` on the result and saved ourselves a whole load of intellectual soup. Sure. I get it. Terrible example. I've let us both down. I'm sorry.

Let's see if we can find a slightly more helpful example.

```haskell
data CalcFace a = CalcFace [String] a deriving (Eq, Show)
```

This data type is for describing calculations. The `a` contains the current total, and the `[String]` is used to describe what is happening. We've deriving standard `Eq` and `Show` instances for it, plus making our own fairly basic `Functor` instance that, as you might expected, maps the function `f` over any `a` inside the datatype and leaves everything else alone like a good functor should.

```haskell
instance Functor CalcFace where
   fmap f (CalcFace names a) = CalcFace names $ f a
```

We also have this function, which we can use to print out the answer with our working.

```haskell
showCalculation :: (Show a) => CalcFace a -> String
showCalculation (CalcFace names a) =
        concat parts ++ " equals " ++ show a where
        parts = intersperse " " names
```

Let's try it out.

```haskell
one :: CalcFace Int
one = CalcFace ["1"] 1

showCalculation one
-- == "1 equals 1"
```

Seems very reasonable.

Let's try some mapping.

```haskell
two :: CalcFace Int
two = fmap (+1) one

showCalculation two
-- == "1 equals 2"
```

Not so good. The answer is correct as we ran a `+1` function over the `1` to get `2`, however we were unable to add anything to our log and thus the string output makes no sense. Perhaps `Functor` is not powerful enough for our task.

Let's give Applicative a go. Here's one of those functions in a box we were talking about. This time we can also include a description of what the function does.

```haskell
addOne :: CalcFace (Int -> Int)
addOne = CalcFace ["add 1"] (+1)
```

We'll need an `Applicative` instance for our datatype too. What does that look like?

```haskell
Prelude> :i Applicative
```

```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  GHC.Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
```

There's some pretty interesting jumping off points in there (such as the whole concept of `lifting`, but we'll come back to that), but fortunately we only need to implement two functions for this typeclass.

1. `pure` - which takes an `a` value and returns a sort of default value for that datatype. This is used internally in constructing new values.
2. `<*>` (called `apply` IRL) - this takes one wrapped function and one wrapped value and combines them.

Confused? Me too! Let's give it a pop.

```haskell
instance Applicative CalcFace where
    pure a = CalcFace [] a
    (CalcFace operation f) <*> (CalcFace value a) =
        CalcFace newNames (f a) where
            newNames = value <> operation
```

OK. So `pure` is reasonably straightforward - it just makes the most basic `CalcFace` imaginable and puts whatever `a` we pass inside it. The other function is a bit trickier though. Basically we are taking the function `f` and applying to `a`, but also taking the `[String]` called `operation` and plopping it on the end of the `[String]` named `value`.

Seems weird? Sure. Let's see it in action though.

```haskell
oneAddOne :: CalcFace Int
oneAddOne = addOne <*> one

-- showCalculation oneAddOne == "1 add 1 equals 2"
```

Nice!

```haskell
oneAddOneAddOne :: CalcFace Int
oneAddOneAddOne = addOne <*> oneAddOne

-- showCalculation oneAddOneAddOne == "1 add 1 add 1 equals 3"
```

Great stuff. The key here is that not only have we acted upon the values, the way that `functor` did, but we have also done something to the structure of the datatype too (in this case, adding to the list of operations). This seems trivial when the operators are just basic mathematics, but if the functions we are running are multiple database operations and calls to other servers, then you've got yourself the start of tidy little pure functional logging solution there, son.

Anyway. This barely scratches the surface, but hopefully gives you the start of an intuition for these things. Applicative does lots of things. Go find them!

Further reading:

[Functors, Applicative and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
