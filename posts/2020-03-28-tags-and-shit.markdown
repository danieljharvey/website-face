---
title: Newtypes for Cheap
tags: typescript, newtype
---

So previously we've spoken about the idea of a `newtype` in Haskell. A
`newtype` is a wrapper you can put around a value to distinguish it from other
items. It's common to give value that has it's own domain meaning it's own
`newtype`, for instance:

```haskell
newtype Firstname = Firstname { getFirstname :: String }
  deriving (Eq, Ord, Show)

newtype Surname = Surname { getSurname :: String }
  deriving (Eq, Ord, Show)
```

We create values as such:

```haskell
first :: Firstname
first = Firstname "Bobby"

last :: Surname
last = Surname "Davehead"
```

And unwrap them to use like this:

```haskell
fullName :: Firstname -> Surname -> String
fullName first' last' = getFirstname first' <> " " <> getSurname last'
-- fullName first last == "Bobby Davehead"
```

This would stop different bits of names, that would usually be stored as
strings, getting mixed up. It's neat. What's really nice about it though, as although
these look like records with a single item inside, and in code we treat them as
such, as runtime that shit gets erased and really it's just a `String` being
passed around for `#performance`.

They have two benefits:

* Firstly, if you are a goof like me, you get this wrong all the time and put
   arguments in the wrong order and generally make a mess of things from
10x-ing all the time, and this makes such terrible errors a little trickier to
make.

* Secondly, and much more usefully, it allows us to practice Alexis King's
   [Parse, don't
validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
pattern. It's well worth reading in full, but the very tl;dr is - if I check a
`String` to see if it's a valid name, I should return a type that tells me
that fact, so I a) don't have to check again and b) don't mix it up with the
unchecked version.

Anyway. It all sounds like good stuff, and I have been thinking about the same
patterns in Typescript as that's a place I often find myself these days.

### Prior art

Now a good place to start when trying to reproduce any Haskell / Purescript
concept in Typescript is to crack open [gcanti's
Github](https://github.com/gcanti) where, as usual, he does not disappoint:
[newtype-ts](https://github.com/gcanti/newtype-ts) is a pretty full featured
library for this built on top of his `monocle-ts` lens library.

Now this is all very well, but does sort of require everybody working on your
codebase to buy in (especially as it requires you to use `modify` from
`monocle-ts` to change the values inside, and to explicitly `wrap` and `unwrap`
them). If everybody is into this, go for it, I am a big fan of the `fp-ts`
stack and swear by `io-ts` at the moment. However, I often need something cheap and hacky, that doesn't leak complexity throughout the codebase.

Fortunately, good man and Habito colleague Mateusz (who I cannot find any links to on
the internet, somebody please help me remedy this), came up with a neat way of
tagging types for cheap, which I'd like to share today.

### The 10x solution

Here is the whole library:

```typescript
type Nominal<T> = {
    readonly symbol: T
}

export type Tagged<Tag extends string, A> = A & Nominal<Tag>
```


This means we can make unique tagged versions of regular datatypes like so: 

```typescript
type Firstname = Tagged<"Firstname", string>

type Surname = Tagged<"Surname", string>

type Age = Tagged<"Age", number>
```

The great thing is that when the Javascript is generated from this, we just
end up with plain `string` and `number` values.

Now, let's see what these buy us:

### Stopping idiots like me mixing stuff up

The most basic use is just tagging our types with `as`:

```typescript
const first = "Bobby" as Firstname

const surname = "Davehead" as Surname

const functionThatNeedsSpecificallyFirstname = (firstname: Firstname): string =>
    `Hello, person whose first name is very much ${firstname}`
```

Now, we can make sure only the right tagged value:

```typescript
functionThatNeedsSpecificallyFirstname(first) // ok!

functionThatNeedsSpecificallyFirstname(surname) // type error!
```

When you have a function like `apiCall(url: string, token: string, accountId:
string) => ...` this really helps to stop you messing stupid things up.

Now, how about that parsing stuff mentioned earlier?

### Parse, don't validate

So, we can also use this tactic to tag data that we have validated, so that we
know certain properties about it have been validated.

Say we have a `Person` type, because I lack imagination:

```typescript
interface Person {
  firstname: string
  surname: string
  age: number
}
```

Now, we want to check a few properties about these things to make sure they're
not terrible.

The naive version of this looks something like:

```typescript
const isValidPerson = (person: Person): boolean => 
  (person.firstname.length > 0 && person.firstname.length < 100)
  && (person.surname.length > 0 && person.surname.length < 50)
  && (person.age >= 0 && person.age < 120)
```

Before using `Person` anywhere, you could check it made sense:

```typescript
if (isValidPerson(person)) {
  doThingWithPerson(person)
} else {
  throw "Person is not valid"
}
```

...and throw an error if it isn't. The thing is, it's nicer to do validation on the boundaries of
our app and then know afterwards that things are OK. What if we could use our `Tagged` type
to help us?

Let's rewrite our validation functions:

```typescript
const validateFirstname = (rawString: string): Firstname | null => 
    (rawString.length > 0 && rawString.length < 100) 
       ? rawString as Firstname : null

const validateSurname = (rawString: string): Surname | null => 
    (rawString.length > 0 && rawString.length < 50) 
       ? rawString as Surname : null

const validateAge = (rawAge: number): Age | null => 
    (rawAge >=0 && rawAge < 120) ? rawAge as Age : null

interface ValidPerson {
    firstname: Firstname
    surname: Surname
    age: Age
}

const validatePerson = (person: Person): ValidPerson | null => {
    const firstname = validateFirstname(person.firstname)
    const surname = validateSurname(person.surname)
    const age = validateAge(person.age)

    return (firstname && surname && age) ?  {
        firstname, surname, age
    } : null
}
```

It looks a bit more verbose, but that's mostly because I've broken each
validation function out. What it means is that we can take our raw `Person`
data, run the validator and get a `ValidPerson` (because it's valid) or `null`
(it wasn't valid).

```typescript
const badPerson = validatePerson({ firstname: "", surname: "", age: -1 })
// null

const goodPerson = validatePerson({ firstname: "Tom", surname: "Hanks", age: 50 })
// { firstname: "Tom", surname: "Hanks", age: 50 }
```

We can then pass `ValidPerson` around the rest of the codebase, knowing that
the values are Good and Valid and not needing to check them again.

### Disclaimer

Now, the disadvantage with such a technique is that there's nothing stopping me
just using `as` to change, say a `Firstname` into a `Surname` and cheating the
whole thing. 

```typescript

const giveMeFirstName = (thanks: Firstname): Firstname => thanks

const surname: "whoa" as Surname

giveMeFirstName(surname as Firstname) // cheating!
```

However, if that's really a problem in your codebase (and I would
delicately suggest that what you have there is more a social and not a coding
problem) - then perhaps it's time to go all in on `newtype-ts` or similar.

Make sense? No? Don't care? [Let me know!](/contact.html)
