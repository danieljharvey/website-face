---
title: A game in Purescript (Part 1 - Semigroups)
tags: purescript, semigroup, monoid
---

Hello! I hope you are well. It has been a little while between posts as I keep starting things and then not finishing them.

My most recent distraction has been remaking a game that I wrote a year or two back called [It Is The Egg](http://itistheegg.com/), which is an HTML canvas game where eggs roll around and generally have a nice time. I wanted to add more levels and features but I have no strong desire to write Typescript in my free time, so I have made the somewhat foolish and time-sucking decision to port it over to Purescript.

![The original game in it's full glory.](/images/it-is-the-egg.png "The original game in it's full glory")

So far I've learned a few things, listed thus:

1. Rewriting an entire game takes a while actually.
2. Using Purescript means about 30% of the LOC it took in Typescript
3. A lot of the problems have already been solved by other people
4. Abstractions are helpful

Anyhow, I figured concrete useful examples are the most difficult thing to find when explaining functional programming, and since I stumbled across a few in this process I figured I'd share some. Today I'm going to example how to describe positions and movement using a `semigroup`.

Meaningless? Possibly! Perhaps some examples...

So we have a `Player` type - it describes one of the eggs onscreen. The eggs have a `position` onscreen, and a `direction` that they are currently moving in.

```haskell
type Player
  = { position  :: Coord
    , direction :: Coord
    }
```

This is the `Coord` type. The eagle-eyed might notice it's a `newtype` rather than a `type` - the reason for this will be explained shortly.

```haskell
newtype Coord = Coord
    { x       :: Int
    , y       :: Int
    , offsetX :: Int
    , offsetY :: Int
    }
```

The game board is a grid, so `position` is a `Coord` where `x` and `y` describe the current square the `Player` is in, and any movement away from the center is described by `offsetX` and `offsetY`. Once a players `position` offset goes over a certain boundary, we increase or decrease `x` or `y` and set `offsetX` or `offsetY` back to zero.

The `position` for the pink egg in the above screenshot would look something like this:

```haskell
position :: Coord
position
  = Coord { x:       3
          , y:       2
          , offsetX: 0
          , offsetY: 32
          }
```

The `direction` is also a `Coord`, where we use `x` and `y` to express which direction the player is headed. Our pink egg above is falling downwards, which would look like this:

```haskell
falling :: Coord
falling
  = Coord { x: 0
          , y: 1
          , offsetX: 0
          , offsetY: 0
          }
```

The `offsetX` and `offsetY` don't really have much use in this context, but that's fine.

So what's the use in expressing two slightly different things in the same data type and also what about that `newtype` and why have we not mentioned `semigroup` yet?

An abridged version of each game turn goes as follows.

1. Look around to see if we are still able to move in direction we want to
2. If not, change direction
3. Move the player in whichever direction we're now decided on

### Looking around the board

OK. So firstly we need to check around the board to see if we are allowed to carry on moving where we are moving. Without going into the whole mechanics of the game board, let's say we have a function that tells us whether a certain `Coord` on the board is a place we are allowed to go.

```haskell
canMove :: Coord -> Boolean
```

What this function does is none of our business at this point - all we need to know is whether the square we intend to move into is happy with our decision to do so.

My first implementation followed the original logic and looked something like this:

```haskell
canIMoveNext :: Player -> Boolean
canIMoveNext player@{ position: Coord pos, direction: Coord dir }
  | dir.x < 0 = canMove ( Coord $ pos { x = pos.x - 1 } )
  | dir.x > 0 = canMove ( Coord $ pos { x = pos.x + 1 } )
  | dir.y < 0 = canMove ( Coord $ pos { y = pos.y - 1 } )
  | dir.y > 0 = canMove ( Coord $ pos { y = pos.y + 1 } )
  | otherwise       = true
```

"If we are moving left then look to the current square but with x reduced once but if we're looking right then look to the current square but with x increased but if we're looking up....".

Quite a laborious thing to read really, and it says nothing of it's intentions.

(If you are not familiar with `guard` syntax, think of this as very similar to `select case` statement in Javascript.)

### Stand back, I am going to use Maths.

OK, so we made that `Coord` a `newtype` for a reason right? Let's add some typeclass instances to it and get it working for us for a change.

First we must please the gods of boilerplate by deriving some standard instances of `Eq`, `Ord` and `Show`. Purescript needs this to be a little more explicit that Haskell does, sadly.

```haskell
derive newtype instance eqCoord :: Eq Coord
derive newtype instance ordCoord :: Ord Coord
derive newtype instance showCoord :: Show Coord
```

`derive newtype instance` means "You know how you're a `newtype` wrapped around something, can you just copy whatever the thing inside does? Ace, thanks."

This means we can now compare and order things, which is helpful although not the main point here.

Let's define a `semigroup`!

```haskell
instance semigroupCoord :: Semigroup Coord where
  append (Coord fst) (Coord snd)
    = Coord { x: fst.x + snd.x
            , y: fst.y + snd.y
            , offsetX: fst.offsetX + snd.offsetX
            , offsetY: fst.offsetY + snd.offsetY
            }
```

(Notice that unlike Haskell, Purescript likes us to give our instances names, hence we have chosen the olympically dull `semigroupCoord`. It could be `pintsOfCream` or `indepedenceDayIsPrettyUnderatedAsFarAsBlockbustersGo`, go absolutely wild if you like.)

So what does this `append` function we have defined do then?

It takes two `Coord` values (`fst` and `snd` in this case) - adds up `x`, `y`, `offsetX` and `offsetY` - then makes and returns a new `Coord`. Therefore we can do stuff like add a `position` and a `direction` together.

In the above example of our pink egg, and our `position` and `falling` values defined above, we can check whether we're OK to keep falling downwards as such.

```haskell
canIMoveDown :: Boolean
canIMoveDown = canMove (position <> falling)
-- canIMoveDown == true (according to the picture)
```

(Note we're using `<>` which is another name for the `append` function, we could just as easily have written `canMove (append position falling)` but I think the little `<>` looks nicer.)

### Looking around the board with Maths

Back to our implementation of `canIMoveNext`, here is the new version that combines our player's `direction` and `position` in a nice tidy way.

```haskell
canIMoveTwo :: Player -> Boolean
canIMoveTwo player
  = canMove (player.position <> player.direction)
```

Nice.

### Changing direction

What if our egg hits a wall or something? What should we do then? It should be as easy as reversing the `direction`, right?

```haskell
invert :: Coord -> Coord
invert (Coord coord)
  = Coord { x: (-1) * coord.x
          , y: (-1) * coord.y
          , offsetX: (-1) * coord.offsetX
          , offsetY: (-1) * coord.offsetY
          }
```

Therefore we can use it on our player like thus:

```haskell
turnAround :: Player -> Player
turnAround player
  = player { direction = invert player.direction }
```

And wrap up the entire logic for the turn like this:

```haskell
updateDirection :: Player -> Player
updateDirection player
  = if canMove player.position
    then player
    else turnAround player
```

(Readers with a keen eye will notice we did not use anything particularly clever or typeclass based here, I just included it for completeness.)

![Another screenshot of the original game to break up the large amount of text.](/images/eggy2.png "Another screenshot of the original game to break up the large amount of text.")

### Actually moving that egg around

Let's get back to moving the egg around. Once we've worked out that our egg is moving somewhere it is allowed to, we need to actually update the `offsetX` and `offsetY` in it's `position` so that's in the new location and ready to go through all this hell again. As web browsers are flaky at best, instead of moving by a set amount the actually amount to increment the change depends on how much time has passed since the last frame. Therefore we need a function that takes the amount to move and a `Player`, and returns a new `Player` that has moved in some way.

Here, in a similar manner to our earlier function, is something like my original painful version.

```haskell
incrementPlayerPosition :: Int -> Player -> Player
incrementPlayerPosition moveAmount player@{ position: Coord pos, direction: Coord dir }
  = player { position = newPosition }
  where
    newPosition
      | dir.x < 0 = Coord $ pos { offsetX = pos.offsetX - moveAmount }
      | dir.x > 0 = Coord $ pos { offsetX = pos.offsetX + moveAmount }
      | dir.y < 0 = Coord $ pos { offsetY = pos.offsetY - moveAmount }
      | dir.y > 0 = Coord $ pos { offsetY = pos.offsetY + moveAmount }
      | otherwise = Coord pos
```

It should work but it's a bit terrifying. Let's break it up and get some of that sweet `semigroup` magic working for us.

Firstly, a helper function - this takes an `Int` and a `Coord` and creates a new `Coord` that describes the movement we want the egg to do.

```haskell
createMoveCoord :: Int -> Coord -> Coord
createMoveCoord amount (Coord coord)
  = Coord { x: 0
          , y: 0
          , offsetX: amount * coord.x
          , offsetY: amount * coord.y
          }
```

Therefore if we take the `falling` value from earlier, and a moving amount of `20`, it would create something like this:

```haskell
downMove :: Coord
downMove
  = Coord { x: 0
          , y: 0
          , offsetX: 0
          , offsetY: 20
          }
```

With our helpful helper function in hand, and a dash of `<>`, we can now rewrite our move function like thus:

```haskell
incrementPlayerDirection :: Int -> Player -> Player
incrementPlayerDirection amount player
  = player { position = newPosition }
  where
    newPosition :: Coord
    newPosition
      = player.position <> moveCoord

    moveCoord :: Coord
    moveCoord
      = createMoveCoord amount player.direction
```

So here we're creating a `Coord` for the movement (called `moveCoord`) - and then using the `<>` function to combine it with the current `position` to make a new `Coord` called `newPosition`, and then making a new `Player` with that `position`.

### Putting it all together

Now we've created all our functions, we can plop them together like this to do the whole move:

```haskell
doMove :: Int -> Player -> Player
doMove amount
  = updateDirection
  >>> incrementPlayerDirection amount
```

Easy as pie.

"But I thought we'd actually draw one of those attractive looking eggs and instead we've just done some shitty maths", I hear you say. OK, OK, sure, I get it, you want adventure, you want action. We'll come to it soon, I promise.
