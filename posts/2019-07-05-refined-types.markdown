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

...gets a string type, which means we don't have to check it exists...

```typescript
function printName(name: string) {
  if (name.length > 0) then {
    return name
  } else {
    return "No name!"
}
```

...but we still need to check whether the string is long enough. Damn.

# What about that classic divide by zero problem?

```javascript
function divide(a, b) {
  if (b == 0 || isNaN(a) || isNaN(b)) then {
    return 0; // would have caused error
  } else {
    return a / b
  }
}
```

- We can get rid of the number checks...

```typescript
function divide(a: number, b: number) {
  if (b == 0) then {
    return 0; // would have caused error
  } else {
    return a / b
  }
}
```

- ...but we've still got to check for that zero value

# What if I told you....

...we could do better than this?

# oh shit

![](./src/oh-shit-2.png)

# Ivory tower bullshit

From now on I may occasionally use the following syntax to describe types:

```haskell
aVariableOrSomething :: TheTypeOfSaidThing
```

- `6` is an `Integer`

```haskell
6 :: Integer
```

- `6.7556` is a `Number`

```haskell
6.7556 :: Number
```

- "Horses" is a `String`

```haskell
"Horses" :: String
```

# Refined types

A Refined type looks like this:

```haskell
Refined predicate value
```

`value` is the type we are refining, for example `Int` or `Number`.

`predicate` is a type that lets us better describe the `value`.

# The most basic Refinement

The `id` predicate, which doesn't really do anything.

```haskell
Refined Id Int
```

- The `id` (or `identity`) function is just a function that returns whatever it
  receives, basically doing nothing.

```haskell
identity :: x -> x
identity x = x
```

- Any value that is a value `Int` can be made into a valid `Refined Id Int`.

```haskell
refine (11233) -- Sure
```

- Great stuff.

```haskell
refine (-213123) -- why not?
```

# A bit more clever

The `Positive` predicate, which only allows numbers over 0.

```haskell
Refined Positive Int
```

- The `refine` function works here:
 
```haskell
refine(10) -- Great
```

- But this isn't going to fly

```haskell
refine(-10) -- NO
```

# We can be more specific

The `From` predicate takes an integer and only allows values equal to or above
it.

```haskell
Refined (From 10) Int
```

- So this will not `refine`.

```haskell
refine(9) -- NO WAY
```

- But this is totally great.

```haskell
refine(100) -- really good stuff
```

# And in balance

The `To` predicate gives us a maximum figure.

```haskell
Refined (To 100) Int
```

- So this is terrible.

```haskell
refine (101) -- NO THANKS
```

- But this is great stuff.

```haskell
refine(80) -- SURE THING
```

# It doesn't just have to be numbers

We can check strings too with `SizeEqualTo`, `SizeGreaterThan` and
`SizeLessThan`.

```haskell
Refined (SizeGreaterThan 3) String
```

- This works...

```haskell
refine "oh" -- nope
```

- But this does

```haskell
refine "well" -- great!
```

# Let's combine them

We've also got `And` and `Or` for combining them.

- This will only allow whole numbers from `1` to `100`.

```haskell
Refined ((From 1) And (To 100)) Int
```

- This will allow all whole numbers EXCEPT `1` to `100`.

```haskell
Refined ((To 0) Or (From 101)) Int
```

- This describes the roll of a dice.

```haskell
type Dice = Refined ((From 1) And (To 6)) Int
```

- Or something rather stupid.

```haskell
type Prime 
  = Refined 
      (Or (Equal 2) 
        (Or (Equal 3) 
          (Or (Equal 5) 
            (Or (Equal 7) 
              (Or (Equal 11) 
                (Or (Equal 13) (Equal 17))
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


