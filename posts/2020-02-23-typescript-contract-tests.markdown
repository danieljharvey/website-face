---
title: Contract testing with Typescript
tags: fastcheck, typescript, io-ts, testing
---

Hello. In our [last article](https://danieljharvey.github.io/posts/2020-02-23-contract-testing.html) we described how to generate test cases for contract
testing in Haskell. This time, we are going to look at generating the front end
portion of these in `Typescript`. This will probably be a lot briefer because I
am a very lazy person at heart.

### First, some rambling

The main differences with the type system of `Haskell` or `Purescript` compared
to that of `Typescript` is that typeclasses allow the types of things to affect
the code that is generated or used. That's how a `newtype` like `Sum` or
`Product` can change the `Monoid` function used to combine two values, even
though both are just `Int` values underneath.

The same thing applied to our `arbitrary` instances - we set a type, derived
the necessary instances, and then all the code was generated for us.

`Typescript` is different. Because all the type info is destroyed at runtime,
we have to create run-time (or `value level`) descriptions of our types, and
then derive the `types` from those. Make sense? No? Doesn't really matter
anyway.

![Whoa, great job!](/images/frisbee-1.png "Whoa, great job!")

### Some datatypes

Fortunately, generating run-time validators and types for them is exactly what
the excellent [io-ts](https://github.com/gcanti/io-ts) is for. It allows to
create validators for our datatypes using runtime functions, but then also
derive types from that. Let's see an example:

```typescript
import * as t from 'io-ts'

export const PetShape = t.type({
  petName: t.string
})

export const UserShape = t.type({
  userId: t.number,
  name: t.string,
  pets: t.array(PetShape)
})

type Pet = t.TypeOf<typeof PetShape>

export type User = t.TypeOf<typeof UserShape>
```

Here we have defined `Pet` and `User` and created validators for them both.
What does this mean?

```typescript
const userJson = { userId: 1, name: "Frank", pets: [] }

const ok = UserShape.decode(userJson))
// ok == Right with a User type inside

const badUserJson = { blah: "dfgdfgdgf" }

const no = UserShape,decode(badUserJson)
// no == Left with an error message
```

This means that we can validate JSON we receive into our application and ensure
that when Typescript says we are talking about a `User`, we really are.

![Really nice!](/images/frisbee-2.png "Really nice!")

### Reading sample JSON

Therefore, hopefully you can work out how we're going to check our generated
backend responses from earlier.

```typescript
import { either } from 'fp-ts'
import * as path from 'path'
import * as fs from 'fs'

// get array of nums 0...99
const rangeArr = [...Array(100).keys()];

// path to wherever our responses are saved
const outputPath = './responses/user/'

describe('Read contract tests', () => {
  test('Write sample responses from files', () => {
    rangeArr.forEach(index => {
      // construct the path
      const filename = path.resolve(`${outputPath}${index}.json`)
      // read the file and turn it into a JS object
      const file = JSON.parse(fs.readFileSync(filename, "utf8"))
      // assert whether it validates properly
      expect(either.isRight(UserShape.decode(file))).toBeTruthy()
    })
  })
})
```

This `Jest` test loads each `.json` file from a given folder and checks that it
decodes correctly with our `UserShape` validator. If they do, then we know, at
least for this particular endpoint, that our frontend and backend agree with
one another.

![You've done this before!](/images/frisbee-3.png "You've done this before!")

### Generating sample JSON

What has traditionally been a bit harder in `Typescript` in generating
`arbitrary` responses. Since we can't use out types to inform which values are
made, we have to be a bit creative.

But that's hard work. What if somebody else had already solved this and we
could just piggyback off their great work?

Enter [fast-check](https://github.com/dubzzz/fast-check) and
[io-ts-fast-check](https://github.com/giogonzo/fast-check-io-ts).

`fast-check` is a property testing tool for `Typescript`. Essentially, for our
purposes, it's `QuickCheck`. `io-ts-fast-check` is the secret sauce that says
"if you can give me a validator, I can generate you random values that satisfy
it."

Therefore, we can use it to create our random front end requests, ready for our
backend to decode and validate. Great!

```typescript
import * as fc from 'fast-check'
import * as fs from 'fs'
import * as path from 'path'
import { getArbitrary } from 'fast-check-io-ts';
import { either } from 'fp-ts'

// this creates a fast-check `Arbitrary` from our `User` validator
const userArbitrary = getArbitrary(UserShape)

// this takes an output folder and an Arbitrary, and fills the folder with 100
// json example files 
export const generate = <A>(outputPath: string, arb: fc.Arbitrary<A>) => {
  fc.sample(arb, 100).map(a => JSON.stringify(a)).map((json, index) => {
    // construct file path for saving the file
    const filename = path.resolve(`${outputPath}${index}.json`)
    // save that goddamn file
    fs.writeFileSync(filename, json)
  })
}

// example of use
generate('./requests/users', userArbitrary)
```

![Feels good, yeah?](/images/frisbee-4.png "Feels good, yeah?")

### Great stuff!

Now we can read our sample requests into the backend and see that everything is
fine. Running tests like this before each deploy of either front or back end
service is a great way to make sure nothing will explode. Source code is
available [here](https://github.com/danieljharvey/ts-contract-tests) if you
can't get it to do what you want for some reason.

Make sense? No? Yes? [Let me know!](/contact.html)
