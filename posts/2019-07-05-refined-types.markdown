---
title: Refine, fine, fine
tags: haskell
---

About a month ago I gave a talk about `Refined` types at a React meetup.
Needless to say, it was a resounding success so I thought I would share an
adapted version of the slides so that you can all learn to be as learned as me
when it comes to such a topic.

### Types 

Do we even care? What are we trying to avoid? What's in it for us?

Let's start by listing some things that we as programmers generally agree we don't particularly like:

### Runtime errors caused by Javascript YOLO

Here is some classic code written in the Javascript programming language:

```javascript
function makeBig(thing) {
  return thing.toUpperCase()
}
```

At a casual glance, it would appear to be some rather convoluted code for
making a string uppercase.

Therefore, this seems fine:

```javascript
makeBig('horse') // 'HORSE'
```

But this isn't so great:

```javascript
makeBig(12) // Error: toUpperCase is not defined
```

What happened? Well it wasn't really a function for making strings uppercase,
it was a function that takes any piece of data, then makes it uppercase if it's
a string, and just breaks weirdly with anything else.

Another thing we don't really like doing as programmers is...

### Overly defensive code around user input

So if your start in programming involved more than a sprinkling of PHP, then
you'll be used to starting all your functions with the `manual typechecking
dance`.

```javascript
function printName(name) {
  if (name && name.length > 0 && typeof name === 'string') then {
    return name
  } else {
    return "No name!"
}
```

### Defense against the dark arts

Another favourite is manually checking our values to check basic mathematical
operators aren't going to explode the whole computer.

```javascript
function divide(a, b) {
  if (b == 0 || isNaN(a) || isNaN(b)) then {
    return 0; // would have caused error
  } else {
    return a / b
  }
}

```
### How far does something like Typescript get us?

So we can change our weird uppercasing function...

```javascript
function makeBig(thing) {
  return thing.toUpperCase()
}
```

...to only take a string like we intended.

```typescript
function makeBig(thing: string) {
  return thing.toUpperCase()
}
```

Let's give it a smash:

```typescript
makeBig('horse') // 'HORSE'
```

Excellent stuff.

And now, if we try and do some wild type stupidity, our code doesn't even compile:

```typescript
makeBig(12) 
// Argument of type '6' is not assignable to parameter of type 'string'.
```

+1

### What about this potentially malformed user input?

This Wild West Cowboy Javascript...

```javascript
function printName(name) {
  if (name && name.length > 0 && typeof name === 'string') then {
    return name
  } else {
    return "No name!"
}
```

...gets a string type, which means we don't have to check that `name` exists...

```typescript
function printName(name: string) {
  if (name.length > 0) then {
    return name
  } else {
    return "No name!"
}
```

...but we still need to check whether `name` is long enough and return a default if not.

### What about that classic divide by zero problem?

What can basic types give us here?

```javascript
function divide(a, b) {
  if (b == 0 || isNaN(a) || isNaN(b)) then {
    return 0; // would have caused error
  } else {
    return a / b
  }
}
```

We can get rid of the number checks...

```typescript
function divide(a: number, b: number) {
  if (b == 0) then {
    return 0; // would have caused error
  } else {
    return a / b
  }
}
```

...but we've still got to check for that zero value. Better, but not great. What if I told you we could do better than this?

### Refined

Enter `Refined` types. A Refined type looks like this:

```haskell
newtype Refined predicate value = Refined value
```

As it is a `newtype` it is a wrapper around a value that is used for type
purposes at compile time but then erased at run time (so when the program runs,
`Refined 100` is just `100` as far as memory etc is concerned)

`value` is the type of actual data we are refining, for example `Int` or `Number`.

`predicate` is a type that lets us better describe the `value`.

The most interesting thing to note here is that `predicate` only exists on the type side (ie before the `=`) and not after - this makes it a `phantom type` which is only used to add contextual information. Let's see what that actually means...

### Making Refined values

There are a few ways to make `Refined` values, especially in the Haskell
library - we'll concentrate on two. I'm going to use the types from the
Purescript version because a) they're simpler and b) I made them and am thus
less likely to get it wrong.

```haskell
refine 
  :: value 
  -> Either RefinedError (Refined predicate value)
```

This is the regular way to make `Refined` value - you pass it a plain value and
it returns either `Left` with a `RefinedError` describing the problem, or `Right` with
the `Refined` value inside.

```haskell
unsafeRefine 
  :: value 
  -> Refined predicate value
```

This ignores the `predicate` and leaves it to the programmer to go full YOLO
and decide whether the predicate will be fine. I have used this to make
`Monoid` classes where I want to add two positive numbers without checking
that the outcome will still be positive.

### Id

The most basic predicate is `id`, which doesn't really do anything.

```haskell
Refined Id Int
```

It's named after the `id` (or `identity`) function - the function that returns whatever it
  receives, basically doing nothing.

```haskell
identity :: x -> x
identity x = x
```

For example, any value that is a value `Int` can be made into a valid `Refined Id Int`.

```haskell
id1 :: Either RefinedError (Refined Id Int)
id1 = refine 11233
-- id1 == Right (Refined 11233)
```

```haskell
id2 :: Either RefinedError (Refined Id Int)
id2 = refine (-213123)
-- id2 == Right (Refined (-213123)
```

### Positive

The `Positive` predicate, which only allows numbers over 0.

```haskell
Refined Positive Int
```

This refinement would pass the predicate:
 
```haskell
positive1 :: Either RefinedError (Refined Positive Int)
positive1 = refine 10
-- positive1 == Right (Refined 10)
```

This clearly very negative number clearly won't fly. Nice try, ding dongs!

```haskell
positive2 :: Either RefinedError (Refined Positive Int)
positive2 = refine (-10)
-- positive2 == Left (GreaterThanError 0 (-10))
```

### From

We can be even more specific with these types too. The `From` predicate takes an integer and only allows values equal to or above
it.

```haskell
Refined (From D10) Int
```

(A note here - that `D10` is a type-level `10`. It is provided by the [purescript-typelevel](https://github.com/bodil/purescript-typelevel) package.)

Therefore this `9` is clearly taking the piss and totally won't `refine`.

```haskell
from1 :: Either RefinedError (Refined (From D10) Int)
from1 = refine 9
-- from1 == Left (FromError 10 9) 
```

However this `100` is cool with me, and will happily `refine`.

```haskell
from2 :: Either RefinedError (Refined (From D10) Int)
from2 = refine 100
-- from2 == Right (Refined 100)
```

### To

Hopefully it should be fairly intuitive how the `To` predicate works...

```haskell
to1 :: Either RefinedError (Refined (To D20) Int)
to1 = refine 21
-- to1 == Left (ToError 20 21)
```

```haskell
to2 :: Either RefinedError (Refined (To D20) Int)
to2 = refine 17
-- to2 == Right (Refined 17)
```

### SizeEqualTo, SizeGreaterThan, SizeLessThan

Refinements don't have to just be about numbers - we can use them on `foldable`
structures too, such as `Lists`. The refinements let us be specific about sizes
of said structure. Therefore we could make a non-empty `List` of `Boolean`
values with `Refined (SizeGreaterThan D0) (List Boolean)`.
 
```haskell
Refined (SizeGreaterThan 3) (List Number)
```

Therefore this list does not `refine`...

```haskell
size1 :: Refined RefinedError (Refined (SizeGreaterThan D3) (List Number))
size1 = refine [1, 2]
-- size1 == Left (SizeGreaterThanError 3 [1, 2])
```

...but this one is fine.

```haskell
size2 :: Refined RefinedError (Refined (SizeGreaterThan D3) (List Number))
size2 = refine [1, 2, 3, 4]
-- size2 == Right (Refined [1, 2, 3, 4])
```

### And, Or

These type signatures are starting to get pretty hefty, but we can do better
than that - we've also got `And` and `Or` for combining them.

Let's only allow whole numbers from `1` to `100`...

```haskell
Refined ((From D1) And (To D100)) Int
```

Or indeed, allow all whole numbers EXCEPT `1` to `100`.

```haskell
Refined ((To D0) Or (From D101)) Int
```

This type describes the roll of a dice.

```haskell
type Dice = Refined ((From D1) And (To D6)) Int
```

Or this one describes the first bunch of prime numbers, and is all a bit silly
to be honest.

```haskell
type Prime 
  = Refined 
      (Or (Equal D2) 
        (Or (Equal D3) 
          (Or (Equal D5) 
            (Or (Equal D7) 
              (Or (Equal D11) 
                (Or (Equal D13) (Equal D17))
              )
            )
          )
        )
      ) Int
```

# So what can we do with them?

# JSON validators

- We could change this data type...

```haskell
type AlcoholUser
  = { name :: String
    , age  :: Int
    }
```

- To use `Refined`...

```haskell
type AlcoholUser
  = { name :: Refined (SizeFrom 1) String
    , age  :: Refined (From 18) Int
    }
```

- ...we can then automatically create validators that include `Refined` types, so our APIs can be fussy and reject bad data before it even makes it into our program.

# Defensive programmming

- This printName function is now unnecessary

```typescript
type Name = Refined (SizeFrom 1) String

function printName(name: Name) {
  return name
}
```

- And this division can now be safe from fear

```typescript
type Divide = Refined (Not (Equal 0)) Number

function divide(a: Number, b: Divide) {
  return a / b
}
```


<!--
-->

# Problem: keeping test data up to date

Let's say we have a data type that we get from a server.

```haskell
type User 
  = { name      :: String
    , age       :: Number
    , likesDogs :: Boolean
    }
```

* Oh shit! It's changed!

 ```haskell
type User 
  = { name     :: String
    , age      :: Number
    , petNames :: Array String
    }
```

* Now where do we need to change it?

# Where does the data live?

- On the server
- Initial / default data in the app
- Every piece of test data
- (hopefully) in the type signature itself

* Changing all those every time our data changes is going to be a 1x pain in
  the arse.
* Especially if we want to make sure they still all agree with one another

# What if....?

We could just auto generate from the types instead?

# That's right

![](./src/oh-shit.png)

# Arbitrary type generation

- We're pretty comfortable with `Math.random()` to get a random number, right?

```
1, 5, 34, 45645, 34534, 345345
```

- So why don't use it to take that `Number`, and get a `Char`?

```
'a', 'e', '{', '>', '^'
```

- And use that to make random `String` values?

```
"sdfsdfsdf", "sdfsdfsdfjhj", "234hj23h324hj"
```

- And take these values and make random `Array` values of them?

```
["sdfsdfsd", "sdfhkhajkrghkjaehkjergeg", "234h234h23iuy4uh2irf"]
```

# OK, but less nonsense.

- Is it such a leap to take our `User` type and generate one of those?

```haskell
type User 
  = { name      :: String
    , age       :: Number
    , likesDogs :: Boolean
    }
```

- Why the hell not? All the fields are pretty straightforward to generate.

```javascript
{ name: "fdjksgjjkfjfsdjskfdgjkljklfdsgjkaeaerg",
  age: 234328948,
  likesDogs: false
}
```

- I confess, usually it ends up more like this

```javascript
{ name: "&n234n2***()()()((()()()()()()()jhsdv87se78&**&&**&A(SD(*A&*SA&*(&*ASDS(&D(SADfd*()A*(D*&(DS&SD&*(&*&(*DSAjksgjjkfjfsdjskfdgjkljklfdsgjkaeaerg",
  age: 232343423448,
  likesDogs: true
}
```

# What for though?

- We could use this values in tests
- (or even do Property Testing - a topic for another time)
- Or indeed to make a quick mock API implementation
- Or test our UIs to make sure they look great with many values inside

# Bringing it all together

- This Arbitrary nonsense gets even more powerful if we bring these `Refined`
  types back into it.

```haskell
type AlcoholUser
  = { name :: String
    , age  :: Int
    }
```

- Then we can automatically generate pretty helpful data

```haskell
type AlcoholUser
  = { name :: Refined (And (SizeFrom 1) (SizeTo 20)) String
    , age  :: Refined (From 18) Int
    }
```

- OK, still not perfect.

```javascript
{ name: "SDJHSDJHHJHJSDHHAhha",
  age: 2389238283
}
```

- But that's just a matter of further refinement.

# Yeah

![](./src/oh-shit-3.png)

# Practical applications for your practical application

- Generating contract tests

- Each of our backend services generates 1000 randomly generated JSON responses on build

- And each frontend generates 1000 randomly generated JSON requests on build

- And before we deploy, each loads the other's responses and makes sure it
  understands all of them

- It's like a PACT contract test, but we don't need to spin up a mock server

- Or really update anything but the code itself

- Nice.

# And what if I told you...

- You could use this method to generate an entire set of API documentation?

# shitttt

![](./src/oh-shit-4.png)

# OK.

- This seems great
- but
- what
- is
- in
- it
- for
- me,
- a
- React
- developer
- that
- only
- really
- came
- here
- for
- pizza
- ?

# Good question

You can have all these things

- In React

- Using Webpack

- `yarn add purescript spago purs-loader`

- `spago init`

- Then crack this into your webpack config:

```javascript
module: {
    rules: [
      {
        test: /\.purs$/,
        exclude: /node_modules/,
        loader: 'purs-loader',
        options: {
          src: [
            'src/**/*.purs'
          ],
          spago: true,
        }
      }
    ]
  }
```

- Create a `Purescript` component using `Purescript-react`

- Then import it with `const { NiceComponent } = import('./purs/niceComponent')`

- And plop it into your jsx.

```javascript
<YourWebApp>
  <NiceComponent />
</YourWebApp>
```

- Profit.

# For real?

- Yeah. We do this. It's really nice.

- Can I use it with `technology x`?

- Sure. Once Webpack has chewed it up it doesn't care how the `React` component
  was made.

- Is Purescript complicated?

- If you want it to be, sure.

- But not for `React` development.

# once more for the back

![](./src/oh-shit-4.png)

# So, in short

- Taxes are good

- Ask more from your public services

# That's all

Any questions?


